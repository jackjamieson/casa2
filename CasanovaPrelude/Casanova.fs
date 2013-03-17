module Casanova.Core

open Casanova.Utilities

let private rule_counter = ref 0
let internal reset_rule_updates() =
  rule_counter := 0
let internal commit_rule_updates() =
  rule_counter := !rule_counter + 1

let internal get_frame_counter () =
  rule_counter.Value

let internal set_frame_counter _rule_counter =
  rule_counter := _rule_counter

/// Store a value in a double buffer. Only the current element can be read, 
/// and only the next element can be written to.
type Rule<'a> =
  struct
    val internal values : 'a[]

    /// Create a rule container with a function that initializes its elements.
    private new (f:unit->'a) = { values = [| f(); f() |] }

    /// Create a rule container with the two elements.
    private new (v1:'a,v2:'a) = { values = [| v1; v2 |] }

    /// Get the current value.
    member this.Value
      with get() = this.values.[rule_counter.Value % 2]
    /// Set the next value.
    member this.SetValue v' =
      this.values.[(rule_counter.Value + 1) % 2] <- v'
    /// Get the current value, additional property
    member this.GetValue = this.Value
    /// Get the current value via a static member
    static member GetValueMember(this:Rule<'a>) = this.Value
    /// Get the next value; do not use directly.
    member this.GetImmediateValue = this.values.[(rule_counter.Value + 1) % 2]
    /// Get the next value via a static member; do not use directly.
    static member GetImmediateValueMember(this:Rule<'a>) = this.GetImmediateValue

    override this.ToString() = 
      "(" +
        this.values.[rule_counter.Value % 2].ToString() + ", " +
        this.values.[(rule_counter.Value + 1) % 2].ToString() + ")"

    /// Internal use only.
    static member Apply (v:Rule<'a>,v') = v.values.[(rule_counter.Value + 1) % 2] <- v'

    /// Internal use only.
    static member Swap(v:Rule<'a>) =
      let x = v.values.[0]
      v.values.[0] <- v.values.[1]
      v.values.[1] <- x

    /// Create a rule from a value type that is duplicated.
    static member Create<'a when 'a : struct> (v:'a) = Rule<'a>(fun () -> v)

    /// Create a rule from a function that returns the value for both current and next.
    static member Create(f:unit->'a) = Rule(f)

    /// Implicitly convert a rule to its immediate value
    static member op_Implicit(r:Rule<'a>) = r.GetImmediateValue

    //Used in the reflection traversal to build a Rule
    static member private CreateReflection(f:unit -> obj) =
      let arg = f() :?> 'a
      Rule(fun () -> arg)
  end


/// Store a list of values in a double buffer. The current list can be only read,
/// while the next list can be only written to. RuleTable is also enumerable; 
/// enumerating it yields the current values.
type RuleTable<'a> =
  struct
    val internal values : ResizeArray<'a>[]

    interface System.Collections.Generic.IEnumerable<'a> with
        member this.GetEnumerator () : System.Collections.Generic.IEnumerator<'a> = this.values.[rule_counter.Value % 2].GetEnumerator() :> System.Collections.Generic.IEnumerator<'a>
        member this.GetEnumerator () : System.Collections.IEnumerator = this.values.[rule_counter.Value % 2].GetEnumerator() :> System.Collections.IEnumerator

    private new (f:unit->'a seq) = { values = [| ResizeArray(f()); ResizeArray(f()) |] }
    private new (s:seq<'a>,s':seq<'a>) = { values = [|ResizeArray(s); ResizeArray(s')|] }

    /// Add an element to the next list of values. To be used exclusively inside scripts.
    member this.Add x =
      this.values.[(rule_counter.Value + 1) % 2].Add x

    /// Clear the next list of values. To be used exclusively inside scripts.
    member this.Clear() =
      this.values.[(rule_counter.Value + 1) % 2].Clear()

    /// Get the current list of values.
    member this.Value
      with get() = this.values.[rule_counter.Value % 2] :> seq<'a>

    /// Set the next list of values.
    member this.SetValue v' =
      let value' = this.values.[(rule_counter.Value + 1) % 2]
      do value'.Clear()
      for x in v' do
        do value'.Add x

    /// Get the current list of values.
    member this.GetValue = this.Value

    /// Get the next list of values. Not to be used explicitly.
    member this.GetImmediateValue = this.values.[(rule_counter.Value + 1) % 2]

    /// Get the current value via a static member
    static member GetValueMember(this:RuleTable<'a>) = 
      this.values.[(rule_counter.Value) % 2]

    /// Get the next list of values. Not to be used explicitly.
    static member GetImmediateValueMember(this:RuleTable<'a>) = 
      this.values.[(rule_counter.Value + 1) % 2]

    /// Get the number of elements already inserted in the next value.
    member this.Count
      with get() = this.values.[(rule_counter.Value + 1) % 2].Count
             
    override this.ToString() = 
      "(" +
        this.values.[rule_counter.Value % 2].ToString() + ", " +
        this.values.[(rule_counter.Value + 1) % 2].ToString() + ")"

    /// Internal use only.
    static member Apply (v:RuleTable<'a>,s) =
      v.Clear()
      for x in s do
        v.Add x

    /// Internal use only.
    static member Swap(v:RuleTable<'a>) =
      let x = v.values.[0]
      v.values.[0] <- v.values.[1]
      v.values.[1] <- x

    /// Create a rule table from a function that returns the initial value of the collection.
    static member Create(f) = RuleTable<'a>(f)

    //Used in the reflection traversal to build a Rule
    static member private CreateReflection(f:unit -> seq<obj>) =
      let arg = f()
      let arg = 
        seq{for x in arg do
              yield x :?> 'a}
      RuleTable<'a>(fun () -> arg)

    /// Implicitly convert a rule table to its current value
    static member op_Implicit(r:RuleTable<'a>) = r.GetImmediateValue

    /// Create an empty rule table.
    static member Empty = RuleTable<'a>(Seq.empty,Seq.empty)
  end

/// Variable.
type Var<'a> = 
  { /// Contents of the variable.
    mutable Value : 'a }
  /// Get the value of the variable.
  member this.GetValue = this.Value
  /// Set the value of the variable.
  member this.SetValue v' = this.Value <- v'
  /// Get the value of the variable.
  member this.GetImmediateValue = this.Value
  /// Create an instance of a variable and initialize it with some contents.
  static member Create(x:'a) = { Value = x }
  /// Implicitly convert a var to its current value
  static member op_Implicit(r:Var<'a>) = r.GetImmediateValue
  /// Internal use only.
  static member Apply (v:Var<'a>,v') = v.Value <- v'
  /// Internal use only.
  static member Swap(v:Var<'a>) = ()

/// Create an instance of a variable and initialize it with some contents.
let var x = Var<'a>.Create x

/// Immutable reference to a value. 
/// References are not traversed and their rules are not applied.
/// References are akin to read-only pointers.
[<StructuralEquality; StructuralComparison>]
type Ref<'a> = 
  { /// Contents of the reference.
    Value : 'a }
  /// Get the value of the variable.
  member this.GetValue = this.Value
  /// Get the value of the variable.
  member this.GetImmediateValue = this.Value
  /// Create an instance of a reference and initialize it with some contents.
  static member Create(x:'a) = { Value = x }
  /// Implicitly convert a ref to its current value
  static member op_Implicit(r:Ref<'a>) = r.GetImmediateValue

/// Create an instance of a reference and initialize it with some contents.
let ref x = Ref<'a>.Create x

//let sync_ref (r : Ref<'a>) : Coroutines.Coroutine<unit> =
//  Coroutines.co{ do! Coroutines.yield_ }

/// Lookup the current value of a rule, rule table, var, or ref.
let inline lookup x = (^a : (member GetValue : ^b) (x))
/// Lookup the next value of a rule or  rule table, or the current value of a var or ref.
let inline immediate_lookup x = (^a : (member GetImmediateValue : ^b) (x))
/// Lookup the current value of a rule, rule table, var, or ref.
let inline (!) x = lookup x
/// Assign the next value of a rule or rule table, and the value of a variable.
let inline assign x v = (^a : (member SetValue : ^b -> Unit) (x,v))
/// Assign the next value of a rule or rule table, and the value of a variable.
let inline (:=) x v = assign x v

type System.Collections.Generic.List<'a>
  /// Get the number of elements of a list.
  with member this.Length = this.Count

//type Table<'a> = ResizeArray<'a>
//let Table<'a>(x:seq<'a>) = ResizeArray<'a>(x)

type RuleList<'a> = RuleTable<'a>

//let add (t:ResizeArray<_>) x = t.Add x

/// Attribute that marks an entity as a Casanova entity which rules should be applied 
/// (and recursively to its attributes). Note that if a datatype accessible through
/// the game world is not marked with this attribute, then both it and its attributes
/// will be ignored by the update and draw functions.
type CasanovaEntity() =
  inherit System.Attribute()
  

type CasanovaWorld() =
  inherit System.Attribute()

/// Attribute that marks an entity as a drawable entity with the
/// methods IdenticalRule, Clear, Register, and Draw. Defining
/// a new CasanovaDrawable allows to support different rendering
/// engines.    
type CasanovaDrawable() =
  inherit System.Attribute()

let mutable private current_id = 0
type NetworkingContext() =
  let mutable id = current_id
  do current_id <- current_id + 1
  member this.ID = id
  

//type PlayerEntity() =
//  inherit CasanovaEntity()

/// Attribute that marks an input event as detected on the client
/// but responded to on the host.
type InputEventDefault() =
  inherit System.Attribute()

/// Attribute that marks an input event as detected on the client
/// and responded to locally on the client.
type InputEventClientOnly() =
  inherit System.Attribute()

//This attributes decide whether this entity is synchronized in multiplayer or not
type LocalProperty() =
  inherit System.Attribute()

type PlayerEntity() =
  inherit System.Attribute()

/// Attribute that marks an input event as detected on the client
/// and responded to both on the host and locally on the client.
type InputEventAll() =
  inherit System.Attribute()

//let immediate_read (v:Rule<'a>) = v.values.[(rule_counter.Value + 1) % 2]
let immediate_write (v:Rule<'a>) v' = v.values.[(rule_counter.Value + 1) % 2] <- v'
//let (<==) = immediate_write



/// Specify how often to update a rule.
type UpdateFrequency = RealTime = 0 | Interactive = 1 | AI = 2


/// Apply this attribute to a rule method in order to specify
/// how often this rule is updated.
type RuleUpdateFrequencyAttribute(frequency:UpdateFrequency) = 
  inherit System.Attribute()
  member this.Frequency = frequency
