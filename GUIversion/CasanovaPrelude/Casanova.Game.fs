module Casanova.Game

open Casanova.Coroutines
open Casanova.Core
open Casanova.Utilities
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Microsoft.FSharp.Reflection
//open Microsoft.Xna.Framework.Input.Touch
open Casanova.Drawing


let private use_async = false
let mutable do_not_draw = false
let mutable use_emit = true
let mutable print_timing = false

type Stack<'a> = System.Collections.Generic.Stack<'a>

// Internal use only
let applyRev f s =
  List.ofSeq s |> List.rev |> Seq.iter f

let apply_reversed_while<'a> (f : 'a -> unit) (cond : 'a -> bool) (s:Stack<'a>) =
  let mutable c = s.GetEnumerator()
  let rec apply_reversed (c:Stack<'a>.Enumerator) =
    let mutable c = c
    if c.MoveNext() then
      let curr = c.Current
      if cond curr then
        apply_reversed c
      f curr
  do apply_reversed c

let update_draw_reload (res : obj) =
  let elems = FSharpValue.GetTupleFields(res)
  let update = elems.[0] :?> (float32 -> unit)
  let draw = elems.[1] :?> (unit -> unit)
  let reload = elems.[2]
  let re_t = (reload.GetType().GetMethods() )
  update, draw, (reload, re_t.[0])
//   :?> (float32 -> unit) * (unit -> unit) * (obj -> ContentManager -> GraphicsDevice -> unit)

/// Type record uaed as arg to the start_game function containing the 
/// GraphicDevice, ContentManager, exit function, save function, load function and DefaultLayer.
type StartGameArgs = 
  {
    //[<System.NonSerialized>]
    graphics_device   : GraphicsDevice
    [<System.NonSerialized>]
    mutable content           : ContentManager
    exit              : unit->unit
    quit              : unit -> Coroutine<unit>
    save              : string -> Coroutine<unit>
    load              : string -> Coroutine<unit>
    set_stack         : (StartGameArgs -> obj * Coroutine<unit> * List<Coroutine<Unit>>) -> Unit
    push_stack        : (StartGameArgs -> obj * Coroutine<unit> * List<Coroutine<Unit>>) * bool -> Unit
    pop_stack         : Unit -> Unit
    default_layer     : SpriteLayer
  }
  member this.SetStack<'a> (start_game: StartGameArgs -> 'a * Coroutine<unit> * List<Coroutine<Unit>>) =
    let start_game s =  
      let w,m,i = start_game s
      in w :> obj,m,i
    this.set_stack  start_game
  member this.PushStack<'a> (start_game: StartGameArgs -> 'a * Coroutine<unit> * List<Coroutine<Unit>>, exclusive_draw) =
    let start_game s =  
      let w,m,i = start_game s
      in w :> obj,m,i
    this.push_stack (start_game, exclusive_draw)
  member this.PopStack<'a> () =
    this.pop_stack()
  /// Internal use only : Create a StartGameArgs object from the values of its field.
  static member internal Create (graphicsDevice, content, exit, save, load, push_stack, pop_stack, set_stack, default_layer) =
    {
      graphics_device   = graphicsDevice
      content           = content
      exit              = exit
      quit              = fun() -> co{ exit() }
      save              = fun name -> co{ 
                                            do! yield_ 
                                            do save name }
      load              = fun name -> co{ load name }
      push_stack        = push_stack
      pop_stack         = pop_stack
      set_stack         = set_stack
      default_layer     = default_layer
    }
  /// Internal use only :  called after deserialization to restore its non-serializable attributes. 
  member internal this.Reload(content : ContentManager) =
    this.content <- content

// Internal use only: a game instance to be putted in the stack.
type internal CurrentInstanceElements =
  {
    [<System.NonSerialized>]
    mutable draw_world    : Unit -> Unit
    [<System.NonSerialized>]
    mutable update_world  : float32 -> Unit
    start_game_args       : StartGameArgs
    world                 : obj
    main                  : Var<Coroutine<Unit>>
    exclusive_draw        : bool
    mutable scripts       : ScriptSet * ScriptSet * ScriptSet 
    mutable rule_counter  : int
  }
  static member create_new draw update start_args world main scripts_ rule_counter exclusive_draw =
    {
      draw_world = draw
      update_world = update
      start_game_args = start_args
      world = world
      main = main
      rule_counter = rule_counter
      scripts = scripts_
      exclusive_draw = exclusive_draw
    }

/// Create a Casanova game; the start_game function initializes the world, the main script, and the input scripts; resolution_x and resolution_y are the requested 
/// screen resolution, and fullscreen determines whether or not to initialize the game window in fullscreen. Invoke the Run method to launch the game after its
/// construction.
type Game(start_game : StartGameArgs -> obj * Coroutines.Coroutine<Unit> * List<Coroutines.Coroutine<Unit>>, 
                  resolution_x, resolution_y, fullscreen, name, ai_tick_interval : float32<s>, input_tick_interval : float32<s>) as this =

  do System.Threading.Thread.CurrentThread.CurrentCulture <- System.Globalization.CultureInfo.GetCultureInfo("en-US")
  do System.Threading.Thread.CurrentThread.CurrentUICulture <- System.Globalization.CultureInfo.GetCultureInfo("en-US")

  /// Singleton used to store the current instance of the game
  static let mutable current_game : option<Game> = None  
  
  [<DefaultValue>]
  val mutable internal stack : Stack<CurrentInstanceElements>

  [<DefaultValue>]
  val mutable graphics : GraphicsDeviceManager
  [<DefaultValue>]
  val mutable update_and_draw_world_async : float32 -> Unit
  [<DefaultValue>]
  val mutable default_layer : SpriteLayer
  [<DefaultValue>]
  val mutable private graphics_device_serializable : GraphicsDevice
  [<DefaultValue>]
  val mutable private just_loaded : bool

  inherit Microsoft.Xna.Framework.Game()
    do this.graphics <- new GraphicsDeviceManager(this)
    do this.IsFixedTimeStep <- false
    do this.graphics.SynchronizeWithVerticalRetrace <- false
  //    do this.graphics.PreferMultiSampling <- true
    do this.graphics.PreferredBackBufferWidth <- resolution_x
    do this.graphics.PreferredBackBufferHeight <- resolution_y
    do this.graphics.IsFullScreen <- fullscreen
    do this.IsMouseVisible <- true
    do this.Window.Title <- name
    do this.Content.RootDirectory <- "Content"
    do current_game <- Some this
    do this.just_loaded <- false
    do this.stack <- Stack<CurrentInstanceElements>()
    //do TouchPanel.EnabledGestures <- GestureType.Tap ||| GestureType.DoubleTap ||| GestureType.FreeDrag ||| GestureType.DragComplete

    /// Internal use only: create a geme instance element.
    member private game.create_instance (start_game : StartGameArgs -> 'a * Coroutine<unit> * List<Coroutine<unit>>) exclusive_draw =
      do reset_rule_updates()
      do clear_ai_scripts()
      do clear_input_scripts()
      do clear_scripts()
      do Casanova.PreTraverse.reset_type_dictionaries()

      let start_game_args = StartGameArgs.Create(this.graphics_device_serializable, this.Content, Game.exit, Game.save, Game.load, Game.PushStack, Game.PopStack, Game.SetStack, this.default_layer)
      let (world, main, input) = start_game(start_game_args)
      let mutable game_world = world :> obj
      do commit_rule_updates()

      let input = [ for i in input do yield repeat_ i ]
      for i in input do run_input_script i
      let main = var main
      let mutable game_main = main
//      do this.update_world <- SlowStateTraversal.update_world world main
//      do this.draw_world   <- SlowStateTraversal.draw_world this.default_layer world
      let mutable update_world = fun (_ : float32) -> ()
      let mutable draw_world = fun () -> ()
      if use_emit then
        let assembly = System.Reflection.Assembly.GetExecutingAssembly()
        let t = assembly.GetType("Casanova.EmitTraversal")
        let mi = t.GetMethod("update_and_draw").MakeGenericMethod [| world.GetType() |]
        let res = mi.Invoke(null, [| (ai_tick_interval |> float32); (input_tick_interval |> float32); this.default_layer; world; main|])
        let update,draw,_ = update_draw_reload res
//        let update,draw,reload = res :?> (float32 -> unit) * (unit -> unit) * (obj -> ContentManager -> GraphicsDevice -> unit)
//        let update,draw,reload = EmitTraversal.update_and_draw (ai_tick_interval |> float32) (input_tick_interval |> float32) this.default_layer world main
        do update_world <- update
        do draw_world   <- if do_not_draw then fun _ -> () else draw
      else
        do update_world <- CachedStateTraversal.update_world (ai_tick_interval |> float32) (input_tick_interval |> float32) world main
        do draw_world   <- if do_not_draw then fun _ -> () else CachedStateTraversal.draw_world this.default_layer world

      if print_timing then
        let update = update_world
        let draw = draw_world
        let timer = System.Diagnostics.Stopwatch()
        let t = var 0.0
        let i = var 0
        do update_world <- 
            (fun dt -> 
              do timer.Restart()
              update dt
              do timer.Stop()
              do t := !t * 0.99 + timer.Elapsed.TotalMilliseconds * 0.01
              do i := !i + 1
              if !i % 100 = 0 then
                do printfn "Stack count :%d" this.stack.Count
                do printfn "time per update/draw = %f" !t)
        do draw_world <- 
            (fun dt -> 
              do timer.Restart()
              draw dt
              do timer.Stop()
              do t := !t * 0.99 + timer.Elapsed.TotalMilliseconds * 0.01)

//      do this.update_and_draw_world_async <- CachedStateTraversal.update_and_draw_world_async world this.default_layer main 
      do try System.Console.Clear() with _ -> ()
      CurrentInstanceElements.create_new draw_world update_world start_game_args game_world main (Coroutines.get_Scripts()) (Casanova.Core.get_frame_counter()) exclusive_draw

    /// Initialize the game by invoking start_game, storing the world, main script and input scripts
    /// as the current values, and by creating (via cached reflection) the draw and update functions 
    /// based on the shape of the 'world type.
    override game.Initialize() =
//      let form = System.Windows.Forms.Control.FromHandle(this.Window.Handle) 
//      let form = form :?> System.Windows.Forms.Form
//      do form.Location <- new System.Drawing.Point(0, 0)      
      do reset_rule_updates()
      do clear_ai_scripts()
      do clear_input_scripts()
      do clear_scripts()
      do Casanova.PreTraverse.reset_type_dictionaries()

      if this.stack.Count < 1 then
        do this.graphics_device_serializable <- new GraphicsDevice(this.GraphicsDevice)
        do this.default_layer <- SpriteLayer.Create(this.graphics_device_serializable, this.Content, BlendState.AlphaBlend)
      let game_instance = game.create_instance start_game true
      do game.stack.Push game_instance
      do base.Initialize()
    
    /// Update the game world by invoking the generated update function once.
    override game.Update(gameTime) =
      if use_async |> not then
        let dt = gameTime.ElapsedGameTime.TotalSeconds |> float32 |> min 0.2f
        let dt = dt * time_speed
        do this.stack.Peek().update_world dt
      do base.Update gameTime

    /// Draw the game world by invoking the generated draw function once.
    override game.Draw(gameTime) =
      do this.GraphicsDevice.Clear(Color.Black)
      if use_async |> not then
        if this.stack.Peek().exclusive_draw |> not then
          do this.stack |> apply_reversed_while (fun s_e -> s_e.draw_world()) (fun s_e -> s_e.exclusive_draw |> not)
        else
        do this.stack.Peek().draw_world ()
      else
        let dt = gameTime.ElapsedGameTime.TotalSeconds |> float32 |> min 0.2f
        let dt = dt * time_speed
        do this.update_and_draw_world_async dt
      do base.Draw gameTime

    /// Save the game as "name".sav storing the world, main script, input scripts, other scripts, the default layer and the rule frame counter.
    member game.Save (name : string) = 
      if not game.just_loaded then
        let formatter = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        use stream = new System.IO.StreamWriter(name + ".sav")
        do stream.BaseStream.Position <- 0L
        let current = game.stack.Peek()
        do current.rule_counter <- Casanova.Core.get_frame_counter()
        do current.scripts <- Coroutines.get_Scripts()
        let items_to_save = (*game.world, game.main, *)game.stack, game.default_layer, this.graphics_device_serializable
        do formatter.Serialize(stream.BaseStream, items_to_save)
        do stream.Close()
        do stream.Dispose()
      else
        do game.just_loaded <- false
      
    /// Load the savegame named "name".sav loading the world, main script, input scripts, other scripts, the default layer and the rule frame counter
    /// and setting them as the current values, and by creating (via cached reflection) the draw and update functions 
    /// based on the shape of the 'world type.
    member game.Load (name : string) = 
      if System.IO.File.Exists(name + ".sav") then
        let formatter = new System.Runtime.Serialization.Formatters.Binary.BinaryFormatter()
        use stream = new System.IO.StreamReader(name + ".sav")
        do stream.BaseStream.Position <- 0L
        let stack, default_layer, g_d_serializable = 
          formatter.Deserialize(stream.BaseStream) :?> 
          (Stack<CurrentInstanceElements> * SpriteLayer * GraphicsDevice)
        do stream.Close ()
        do stream.Dispose ()
        do g_d_serializable.Reload game.GraphicsDevice
        do game.graphics_device_serializable <- g_d_serializable
        do Coroutines.set_Scripts (stack.Peek().scripts)
        do Casanova.Core.set_frame_counter (stack.Peek().rule_counter)
        do default_layer.Reload(game.Content, this.graphics_device_serializable)
        do game.default_layer <- default_layer
//        do game.start_game_args.Reload(this.Content)
        let reload_instance game_inst = 
          do game_inst.start_game_args.Reload(game.Content)
          if use_emit then
            // via reflection        
            let assembly = System.Reflection.Assembly.GetExecutingAssembly()
            let t = assembly.GetType("Casanova.EmitTraversal")
            let mi = t.GetMethod("update_and_draw").MakeGenericMethod [| game_inst.world.GetType() |]
            let res = mi.Invoke(null, [| (ai_tick_interval |> float32); (input_tick_interval |> float32); this.default_layer; game_inst.world; game_inst.main|])
            let update,draw,(reload, reload_invoker) = update_draw_reload res
            let res = reload_invoker.Invoke(reload, [| game_inst.world; game.Content; game.graphics_device_serializable|])
            do game_inst.update_world <- update
            do game_inst.draw_world   <- if do_not_draw then fun _ -> () else draw
//            do draw ()
          else
            do CachedStateTraversal.reload_layers game_inst.world game.Content game.graphics_device_serializable
            do CachedStateTraversal.reload_drawables game_inst.world
            do game_inst.update_world  <- CachedStateTraversal.update_world (ai_tick_interval |> float32) (input_tick_interval |> float32) game_inst.world game_inst.main
            do game_inst.draw_world    <- CachedStateTraversal.draw_world this.default_layer game_inst.world
        
        do apply_reversed_while reload_instance (fun _ -> true) stack
        do game.stack <- stack
        do game.just_loaded <- true
    
    /// Pushes to the top of the stack the game instance created invoking the start game function
    member game.PushStack(start_game : StartGameArgs -> obj * Coroutine<unit> * List<Coroutine<unit>>, exclusive_draw) = 
      let current = game.stack.Peek()
      current.rule_counter <- Casanova.Core.get_frame_counter()
      current.scripts <- Coroutines.get_Scripts()
      Coroutines.set_Scripts (ScriptSet.Zero, ScriptSet.Zero, ScriptSet.Zero)
      let game_instance = game.create_instance start_game exclusive_draw
      do game.stack.Push game_instance
    
    /// Removes the top game instance on the stack
    member game.PopStack () = 
      if game.stack.Count > 1 then
        game.stack.Pop() |> ignore
        Coroutines.set_Scripts (game.stack.Peek()).scripts
        Casanova.Core.set_frame_counter (game.stack.Peek().rule_counter)
      else
        Game.exit()

    /// Set the game instance created invoking the start game function as only element in the stack removing others
    member game.SetStack start_game = 
      do game.stack.Clear()
      do Coroutines.set_Scripts (ScriptSet.Zero, ScriptSet.Zero, ScriptSet.Zero)
      let game_instance = game.create_instance start_game true
      do game.stack.Push game_instance
    
    /// load function on the singleton current_game passed to the start_game function.
    static member load (name) = 
      current_game.Value.Load (name)
    
    /// save function on the singleton current_game passed to the start_game function.
    static member save (name) = 
      current_game.Value.Save (name)

    /// exit function on the singleton current_game passed to the start_game function.
    static member exit () =
      current_game.Value.Exit ()

    /// PushStack function on the singleton current_game passed to the start_game function.
    static member PushStack (start_game : StartGameArgs -> obj * Coroutine<unit> * List<Coroutine<unit>>, exclusive_draw) = 
      current_game.Value.PushStack (start_game, exclusive_draw)

    /// PopStack function on the singleton current_game passed to the start_game function.
    static member PopStack () =
      current_game.Value.PopStack()

    /// SetStack function on the singleton current_game passed to the start_game function.
    static member SetStack start_game = 
      current_game.Value.SetStack start_game

    /// Create a game from the start_game function and optionally x resolution, y resolution, fullscreen option, ai tick interval and input tick interval.
    static member Create<'a>(start_game : StartGameArgs -> 'a * Coroutines.Coroutine<Unit> * List<Coroutines.Coroutine<Unit>>, 
                         ?resolution_x, ?resolution_y, ?fullscreen, ?name, ?ai_tick_interval, ?input_tick_interval ) =
        let resolution_x = defaultArg resolution_x 1024
        let resolution_y = defaultArg resolution_y 768
        let fullscreen = defaultArg fullscreen false
        let name = defaultArg name "MyGame"
        let ai_tick_interval = defaultArg ai_tick_interval 0.2f<s>
        let input_tick_interval= defaultArg input_tick_interval 0.05f<s>
        let start_game s =  
          let w,m,i = start_game s
          in w :> obj,m,i
        new Game(start_game, resolution_x, resolution_y, fullscreen, name, ai_tick_interval, input_tick_interval)

    (*static member Create(start_game : GraphicsDevice * ContentManager * (Unit -> Unit) * SpriteLayer -> 'world * Coroutines.Coroutine<Unit> * List<Coroutines.Coroutine<Unit>>, 
                         resolution_x, resolution_y, fullscreen, name, ai_tick_interval, input_tick_interval) =
        new Game<'world>(start_game, resolution_x, resolution_y, fullscreen, name, ai_tick_interval, input_tick_interval)

    static member Create(start_game : GraphicsDevice * ContentManager * (Unit -> Unit) -> 'world * Coroutines.Coroutine<Unit> * List<Coroutines.Coroutine<Unit>>, 
                         ?resolution_x, ?resolution_y, ?fullscreen, ?name, ?ai_tick_interval, ?input_tick_interval ) =
        let resolution_x = defaultArg resolution_x 1024
        let resolution_y = defaultArg resolution_y 768
        let fullscreen = defaultArg fullscreen false
        let name = defaultArg name "MyGame"
        let ai_tick_interval = defaultArg ai_tick_interval 0.2f<s>
        let input_tick_interval= defaultArg input_tick_interval 0.05f<s>
        let start_game(dev,con,exit,def) = start_game(dev,con,exit)
        Game<'world>.Create(start_game, resolution_x, resolution_y, fullscreen, name, ai_tick_interval, input_tick_interval)*)
