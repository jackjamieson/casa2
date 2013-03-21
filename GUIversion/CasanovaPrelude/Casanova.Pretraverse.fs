module Casanova.PreTraverse

open Casanova.Core
open Casanova.Utilities
open Casanova.Coroutines
open System
open System.Reflection
open TypePredicates
open Casanova.Drawing
open Microsoft.FSharp.Reflection
open Casanova.Action

let internal is_frequency_ok attributes frequency =
  let has_frequency f (x:obj) =
    match x with
    | :? RuleUpdateFrequencyAttribute as x -> x.Frequency = f
    | _ -> false
  match frequency with
  | UpdateFrequency.AI -> attributes |> Seq.exists (has_frequency UpdateFrequency.AI)
  | UpdateFrequency.Interactive -> attributes |> Seq.exists (has_frequency UpdateFrequency.Interactive)
  | UpdateFrequency.RealTime -> 
    attributes |> Seq.exists (has_frequency UpdateFrequency.RealTime) ||
    (attributes |> Seq.forall (has_frequency UpdateFrequency.AI >> not) &&
     attributes |> Seq.forall (has_frequency UpdateFrequency.Interactive >> not))
  | _ -> false


let internal types_with_draw = System.Collections.Generic.Dictionary<System.Type, bool>()
let internal types_with_realtime_rules = System.Collections.Generic.Dictionary<System.Type, bool>()
let internal types_with_interactive_rules = System.Collections.Generic.Dictionary<System.Type, bool>()
let internal types_with_ai_rules = System.Collections.Generic.Dictionary<System.Type, bool>()
let internal types_with_actions = System.Collections.Generic.Dictionary<System.Type, bool>()
let internal all_types = System.Collections.Generic.Dictionary<string, System.Type>()



let internal reset_type_dictionaries() =
  types_with_draw.Clear()
  types_with_realtime_rules.Clear()
  types_with_ai_rules.Clear()
  types_with_interactive_rules.Clear()
  types_with_actions.Clear()
  all_types.Clear()

let private safe_add (k,v) (this:System.Collections.Generic.Dictionary<System.Type, bool>) = 
    if this.ContainsKey(k) |> not then
      this.Add(k,v)
    elif this.[k] = false then
      this.Remove(k) |> ignore
      this.Add(k,v)

let rec internal pre_traverse_entity compute_draw update_frequency (types_with_rules:System.Collections.Generic.Dictionary<System.Type, bool>) types_traversed (t_self:System.Type) = 
  
  if all_types.ContainsKey(t_self.Name) |> not then
    all_types.Add(t_self.Name, t_self)
    

  let is_ref = match t_self with | RefType(t_self) -> true | _ -> false
  let is_cnv = t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaEntity>)
  let types_traversed =
    if not is_ref && is_cnv then
      if types_traversed |> Seq.exists ((=) t_self) then 
        failwithf "Type %s has been encountered already in %A" 
                  t_self.Name 
                  [for t in types_traversed do yield t.Name]
      t_self :: types_traversed
    else
      types_traversed
//    printfn "%A" [for t in types_traversed do yield t.Name]
  let pre_traverse_type_list (t_self:System.Type) (l:List<System.Type>) =
    let mutable has_rules = false
    let mutable has_draw = false
    let mutable has_action = false

    for t in l do
      do pre_traverse_entity compute_draw update_frequency types_with_rules types_traversed t
      if types_with_draw.[t] then has_draw <- true
      if types_with_rules.[t] then has_rules <- true
      if types_with_actions.[t] then  has_action <- true
    if compute_draw then
      types_with_draw |> safe_add(t_self, has_draw)
    types_with_rules |> safe_add(t_self, has_rules)
    types_with_actions |> safe_add(t_self, has_action)

  if (types_with_draw.ContainsKey(t_self) && types_with_rules.ContainsKey(t_self)) |> not then

    match t_self with
    | Tuple(args) ->
      pre_traverse_type_list t_self args
    | RefType(arg) ->
      if compute_draw then
        types_with_draw |> safe_add(t_self,false)
      types_with_rules |> safe_add(t_self,false)
      types_with_actions |> safe_add(t_self,false)

    | RuleTableType(list_arg) ->
      pre_traverse_type_list t_self [list_arg]    
    | ListType(list_arg) -> 
      pre_traverse_type_list t_self [list_arg]    
    | ArrayType(list_arg) -> 
      pre_traverse_type_list t_self [list_arg]    
    | UnionType(cases)->
      let inner_types = [for case in cases do for field in case.GetFields() do yield field.PropertyType]
      pre_traverse_type_list t_self inner_types
    | OptionType(option_arg) ->
      pre_traverse_type_list t_self [option_arg]
    | VarType(arg) | RuleType(arg) ->
      pre_traverse_type_list t_self [arg]
    | _  when t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) ->
      if compute_draw then
        types_with_draw |> safe_add(t_self,true)
      types_with_rules |> safe_add(t_self,false)
      types_with_actions |> safe_add(t_self,false)

    | _  when t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaWorld>) ->
      let inner_types = 
        [
          for field in t_self.GetProperties() do
            if field.GetCustomAttributes(false) |> Seq.exists (fun a -> a.GetType() = typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>) then
              if field.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<ActionAttribute>) then
                types_with_actions |> safe_add(t_self, true)
              yield field.PropertyType
        ]
      pre_traverse_type_list t_self inner_types
      let methods = t_self.GetMethods() |> Seq.filter (fun m -> m.Name.EndsWith("Rule") || m.Name.EndsWith("'"))      
      if methods |> Seq.isEmpty |> not then
        if methods |> Seq.exists (fun m -> is_frequency_ok (m.GetCustomAttributes(false)) update_frequency) then
          types_with_rules |> safe_add(t_self,true)

    | _  when t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaEntity>) ->
      let inner_types = 
        [
          for field in t_self.GetProperties() do
            if field.GetCustomAttributes(false) |> Seq.exists (fun a -> a.GetType() = typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>) then              
              if field.PropertyType.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<ActionAttribute>) then
                types_with_actions |> safe_add(t_self, true)
              yield field.PropertyType
        ]
      pre_traverse_type_list t_self inner_types
      let methods = t_self.GetMethods() |> Seq.filter (fun m -> m.Name.EndsWith("Rule") || m.Name.EndsWith("'"))      
      if methods |> Seq.isEmpty |> not then
        if methods |> Seq.exists (fun m -> is_frequency_ok (m.GetCustomAttributes(false)) update_frequency) then
          types_with_rules |> safe_add(t_self,true)
    | _ ->
      if compute_draw then
        types_with_draw |> safe_add(t_self,false)
      types_with_rules |> safe_add(t_self,false)
      types_with_actions |> safe_add(t_self, false)