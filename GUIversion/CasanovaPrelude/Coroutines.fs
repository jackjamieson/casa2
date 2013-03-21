module Casanova.Coroutines

open Microsoft.FSharp
open Microsoft.FSharp.Core
open System
open Utilities
open Input

type Coroutine<'a> = Unit -> CoroutineStep<'a>
and CoroutineStep<'a> =
  Return of 'a
  | Yield of Coroutine<'a>
  | ArrowYield of Coroutine<'a>

type CoroutineBuilder() =
  member this.Return(x:'a) : Coroutine<'a> =
    fun s -> Return x
//    member this.Yield(x:Unit) : Coroutine<Unit> =
//      fun s -> Yield (fun s -> Return ())    
  member this.Bind(p : Coroutine<'a>,
                    k : 'a -> Coroutine<'b>) : Coroutine<'b> =
    fun s ->
      match p s with
      | Return x -> k x s
      | Yield p' -> Yield(this.Bind(p',k))
      | ArrowYield p' -> ArrowYield(this.Bind(p',k))
  member this.Combine(p1:Coroutine<'a>,
                      p2:Coroutine<'b>) : Coroutine<'b> =
    this.Bind(p1, fun _ -> p2)
  member this.For(items:seq<'a>, body:'a -> Coroutine<Unit>) : Coroutine<Unit> =
    this.ForList(items |> Seq.toList, body)
  member this.ForList(items:list<'a>, body:'a -> Coroutine<Unit>) : Coroutine<Unit> =
    fun s ->
      match items with
      | [] -> Return()
      | hd :: tl -> 
        this.Combine(body hd, this.ForList(tl, body)) s
  member this.Zero() : Coroutine<Unit> = this.Return()
  member this.ReturnFrom(s:Coroutine<'a>) = s
  member this.Delay s = this.Bind(this.Return(), s)
  member this.Run s = s
//  [<CustomOperation("when")>]
//  member this.When(s:Coroutine<'a>, p:Unit -> bool) : Coroutine<'a> =
//    if p() then
//      s
//    else
//      let y = fun s -> Yield(fun s -> Return())
//      let k = fun () -> this.When(s,p)
//      this.Bind(y, k)


/// Create a coroutine.
let co = CoroutineBuilder()

/// Suspend a coroutine for a tick of the main loop.
let yield_       : Coroutine<Unit> = fun s -> Yield(fun s -> Return())

let private arrow_yield_ : Coroutine<Unit> = fun s -> ArrowYield(fun s -> Return())

/// Ignore the result of a coroutine, and return Unit instead of it.
let ignore_ (s:Coroutine<'a>) : Coroutine<Unit> =
  co{
    let! _ = s
    return ()
  }

/// Run two coroutines in parallel; first one to terminate kills the other one.
let rec (.&&) (s1:Coroutine<'a>) (s2:Coroutine<'b>) : Coroutine<'a * 'b> =
  fun s ->
    match s1 s,s2 s with
    | Return x, Return y -> Return(x,y)
    | Yield k1, Yield k2
    | Yield k1, ArrowYield k2
    | ArrowYield k1, Yield k2
    | ArrowYield k1, ArrowYield k2 -> (.&&) k1 k2 |> Yield
    | Yield k1, Return y
    | Yield k1, Return y
    | ArrowYield k1, Return y
    | ArrowYield k1, Return y -> (.&&) k1 (co{return y}) |> Yield
    | Return x, Yield k2
    | Return x, ArrowYield k2
    | Return x, Yield k2
    | Return x, ArrowYield k2 -> (.&&) (co{return x}) k2 |> Yield

/// Run two coroutines concurrently; first one to terminate kills the other one.
let rec (.||) (s1:Coroutine<'a>) (s2:Coroutine<'b>) : Coroutine<Choice<'a,'b>> =
  fun s ->
    match s1 s,s2 s with
    | Return x, _        -> Return(Choice1Of2 x)
    | _, Return y        -> Return(Choice2Of2 y)
    | ArrowYield k1, _   ->
      co{
        let! res = k1
        return Choice1Of2 res
      } |> Yield
    | _, ArrowYield k2   ->
      co{
        let! res = k2
        return Choice2Of2 res
      } |> Yield
    | Yield k1, Yield k2 -> (.||) k1 k2 |> Yield

/// Run two coroutines in parallel, and discard their result.
let (.||>) s1 s2 = ignore_ (s1 .|| s2)

/// Alias for combine
let (=>>) (c:Coroutine<Unit>) (s:Coroutine<'a>) : Coroutine<'a> =
  co.Combine(c,s)

/// Run a coroutine only after a guard coroutine succeeds and returns true; until that
/// happens, keep running the guard coroutine.
let rec (=>) (c:Coroutine<bool>) (s:Coroutine<'a>) : Coroutine<'a> =
  co{
    let! x = c
    if x then
      do! arrow_yield_
      let! res = s
      return res
    else
      do! yield_
      return! (=>) c s
  }

/// Run a coroutine only after a guard coroutine succeeds and returns Some(x); until that
/// happens, keep running the guard coroutine.
let rec (==>) (c:Coroutine<Option<'a>>) (s:'a -> Coroutine<'b> ) : Coroutine<'b> =
  co{
    let! alpha = c
    match alpha with
    | None -> 
      do! yield_
      return! (==>) c s
    | Some x ->
      do! arrow_yield_
      let! res = s x
      return res
  }

/// Run a list of coroutines in parallel, until all of them complete.
let rec parallel_many_ (s:List<Coroutine<Unit>>) : Coroutine<Unit> =
  fun () -> 
    match s with
    | [] -> Return()
    | s -> 
      let s' = [ for c in s do
                  match c() with
                  | Return () -> ()
                  | Yield k | ArrowYield k -> yield k ]
      Yield(parallel_many_ s')

/// Run two coroutines in parallel, until both of them complete.
let (.&&>) s1 s2 = [s1; s2] |> parallel_many_
     
/// Keep running a coroutine.
let rec repeat_ (s:Coroutine<Unit>) : Coroutine<Unit> =
  co{
    do! s
    return! repeat_ s
  }

/// Run an action every tick until a certain interval has elapsed.
let wait_doing (action:float32 -> Coroutine<Unit>) (interval:float32<s>) : Coroutine<Unit> =
  let time : Coroutine<DateTime> = fun _ -> Return(DateTime.Now)
  co{
    let rec wait time_elapsed =
      co{
        let! t0 = time
        do! yield_
        let! t = time
        let dt = (t - t0).TotalSeconds |> float32
        let dt = dt * time_speed
        let time_elapsed = time_elapsed + dt
        if time_elapsed < float32 interval then
          do! action time_elapsed
          return! wait time_elapsed
      }
    do! wait 0.0f
  }

/// Keep ticking until a certain interval has elapsed.
let wait = wait_doing (fun (dt:float32) -> co{ return () })

/// Wait for a certain condition to be met.
let wait_condition p =
  let rec wait() =
    co{
      if p() then
        return ()
      else
        do! yield_
        return! wait()
    }
  wait()
  
type Script = Coroutine<Unit>

let internal update_script (ai:Script) = 
    match ai () with
    | Return () -> yield_
    | Yield k | ArrowYield k -> k

type internal ScriptSet = {
    mutable scripts : List<Coroutine<Unit>>
    mutable added_scripts : List<Coroutine<Unit>>
  } with 
    static member Zero = { scripts = []; added_scripts = [] }
    member scripts.Run(s) =
      scripts.added_scripts <- s :: scripts.added_scripts
    member scripts.Clear() =
      scripts.scripts <- []
      scripts.added_scripts <- []
    member scripts.Step() =
      scripts.scripts <-
        [
          for s in scripts.scripts do match s () with  | Return() -> () | Yield k | ArrowYield k -> yield k
          yield! scripts.added_scripts]
      scripts.added_scripts <- []

let mutable private scripts = ScriptSet.Zero
let mutable private ai_scripts = ScriptSet.Zero
let mutable private input_scripts = ScriptSet.Zero

/// Add a script to the list of active scripts.
let run_script s = scripts.Run(s)

/// Clear the list of active scripts.
let clear_scripts() = scripts.Clear()

/// Update (until they either terminate or evaluate a yield operation) all active scripts.
let step_scripts() = scripts.Step()

/// Add a script to the list of active ai scripts.
let run_ai_script s = ai_scripts.Run(s)

/// Clear the list of active ai scripts.
let clear_ai_scripts() = ai_scripts.Clear()

/// Update (until they either terminate or evaluate a yield operation) all active ai scripts.
let step_ai_scripts() = ai_scripts.Step()

/// Add a script to the list of active input scripts.
let run_input_script s = input_scripts.Run(s)

/// Clear the list of active input scripts.
let clear_input_scripts() = input_scripts.Clear()

/// Update (until they either terminate or evaluate a yield operation) all active input scripts.
let step_input_scripts() = input_scripts.Step()

/// Wait for pressure and then release of a key
let wait_key_press key =
  co{
    do! wait_condition (fun () -> is_key_down key)
    do! wait_condition (fun () -> is_key_up key) //.||> wait 0.2f<s>
  }

  /// Wait for pressure of a key
let wait_key_down key =
  co{
    return is_key_down key
  }

/// Wait for mouse wheel delta
let wait_scroll_wheel delta =
  co{
    let scroll_state0 = Microsoft.Xna.Framework.Input.Mouse.GetState().ScrollWheelValue
    do! wait_condition (fun () -> abs (Microsoft.Xna.Framework.Input.Mouse.GetState().ScrollWheelValue - scroll_state0) >= delta)
    return Some(float32(Microsoft.Xna.Framework.Input.Mouse.GetState().ScrollWheelValue - scroll_state0))
  }

/// Wait for pressure and then release of the left mouse button
let wait_left_mouse_press =
  co{
    do! wait_condition is_mouse_left_down
    do! wait_condition is_mouse_left_up
  }

let wait_left_mouse_down = wait_condition is_mouse_left_down
let wait_left_mouse_up = wait_condition is_mouse_left_up

/// Wait for pressure and then release of the right mouse button
let wait_right_mouse_press =
  co{
    do! wait_condition is_mouse_right_down
    do! wait_condition is_mouse_right_up
  }

  /// Internal use only : Return as tuple three ScriptSet: the scripts, the ai_script, the input_script.
let internal get_Scripts () = 
  scripts, ai_scripts, input_scripts

  /// Internal use only : Restore the three ScriptSet: the scripts, the ai_script, the input_script with the given in input. Problemi di chiusura e inform expert
let internal set_Scripts (_scripts, _ai_scripts, _input_script) =
  scripts <- { scripts = _scripts.scripts; added_scripts = _scripts.added_scripts }
  ai_scripts <- {scripts = _ai_scripts.scripts; added_scripts = _ai_scripts.added_scripts }
  input_scripts <- { scripts = _input_script.scripts; added_scripts = _input_script.added_scripts }
