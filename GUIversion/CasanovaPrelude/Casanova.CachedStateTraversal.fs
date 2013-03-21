module Casanova.CachedStateTraversal

open Casanova.Core
open Casanova.Utilities
open Casanova.Coroutines
open System
open System.Reflection
open TypePredicates
open Casanova.Drawing
open Microsoft.FSharp.Reflection
open Casanova.PreTraverse

type Ref<'a> = Microsoft.FSharp.Core.Ref<'a>
let private (!) (r:Microsoft.FSharp.Core.Ref<'a>) = r.Value
let private (:=) (r:Microsoft.FSharp.Core.Ref<'a>) (v:'a) = r.Value <- v
let private ref = Microsoft.FSharp.Core.Operators.ref


let inline internal timer t max_t dt a = 
  if !t >= max_t then
    do t := !t - max_t
    do a()
  do t := !t + dt


let rec private traverse_entity (t_world : System.Type) (t_self : System.Type) (apply_to_both_rules_values : bool) (k:Ref<obj -> obj -> 'b -> Unit>) (a : System.Type -> System.Type -> Ref<obj -> obj -> 'b -> Unit> -> Unit) (type_predicate:System.Type -> bool) = 
  if type_predicate t_self then
    match t_self with
    | RefType(arg) -> ()

    | Tuple(args) ->

      for i,arg in args |> Seq.mapi(fun i arg -> i + 1,arg) do
        let k_aux = ref (fun w s dt -> ())
        do traverse_entity t_world arg apply_to_both_rules_values k_aux a type_predicate
        let k_aux = !k_aux
        let k' = !k

        let item_i = t_self.GetProperty("Item" + string i)                
        let item_i_get = item_i.GetGetMethod()

        do k := fun world self dt ->
                  k' world self dt
                  let item = item_i_get.Invoke(self, [||])
                  do k_aux world item  dt

    | RuleTableType(list_arg) -> 
      let k_aux = ref (fun w s dt -> ())
      do traverse_entity t_world list_arg apply_to_both_rules_values k_aux a type_predicate
      let k_aux = !k_aux
      let k' = !k
      if apply_to_both_rules_values then
        let get_value = t_self.GetProperty("Value").GetGetMethod()
        let get_value' = t_self.GetProperty("GetImmediateValue").GetGetMethod()
        do k := fun world self dt ->
                  k' world self dt
                  let value = get_value.Invoke(self, [||]) :?> obj seq
                  let value' = get_value'.Invoke(self, [||]) :?> obj seq
                  for x in Seq.append value value' do
                    do k_aux world x dt
      else
        do k := fun world self dt ->
                  k' world self dt
                  for x in  self :?> System.Collections.IEnumerable do
                    do k_aux world x dt
    

    | ListType(list_arg) -> 
      let k_aux = ref (fun w s dt -> ())
      do traverse_entity t_world list_arg apply_to_both_rules_values k_aux a type_predicate
      let k_aux = !k_aux
      let k' = !k
      do k := fun world self dt ->
                k' world self dt
                for x in  self :?> System.Collections.IEnumerable do
                  do k_aux world x dt
    | UnionType(cases)->
      let tag_reader = FSharpValue.PreComputeUnionTagReader(t_self)
      let union_readers = 
        [|
          for case in cases do yield FSharpValue.PreComputeUnionReader(case)
        |]

      let k_parameters = 
        [|
          for case in cases do 
            
            yield
              [
                for field in case.GetFields() do
                  let k_aux = ref (fun w s dt -> ())
                  do traverse_entity t_world field.PropertyType apply_to_both_rules_values k_aux a type_predicate
                  yield !k_aux
              ]
        |]
    
      let k' = !k
      do k := fun world self dt ->
                k' world self dt
                let tag = tag_reader self
                let parameters = union_readers.[tag] self
                for p,k in Seq.zip parameters k_parameters.[tag] do
                  k world p dt
    

    
    | OptionType(option_arg) ->
      let k_aux = ref (fun w s dt -> ())
      do traverse_entity t_world option_arg apply_to_both_rules_values k_aux a type_predicate
      let k_aux = !k_aux
      let k' = !k

      let f_methods = t_self.GetMethods()
      let isSome_prop = t_self.GetProperty("IsSome")
      let isSome_prop_get = isSome_prop.GetGetMethod()
      let parameters = isSome_prop_get.GetParameters()

      let option_value = t_self.GetProperty("Value")                
      let option_value_get = option_value.GetGetMethod()

      do k := fun world self dt ->
                k' world self dt
                if (isSome_prop_get.Invoke(null,[|self|]) :?> bool) then
                  let f = option_value_get.Invoke(self,[||])
                  do k_aux world f dt

    | VarType(arg) ->
      let k_aux = ref (fun w s dt -> ())
      do traverse_entity t_world arg apply_to_both_rules_values k_aux a type_predicate
      let k_aux = !k_aux
      let k' = !k

      let value = t_self.GetProperty("Value")                
      let value_get = value.GetGetMethod()

      do k := fun world self dt ->
                k' world self dt
                let f = value_get.Invoke(self,[||])
                do k_aux world f dt
    | RuleType(arg) ->
      let k_aux = ref (fun w s dt -> ())
      do traverse_entity t_world arg apply_to_both_rules_values k_aux a type_predicate
      let k_aux = !k_aux
      let k' = !k

      let value = t_self.GetProperty("Value")                
      let value_get = value.GetGetMethod()
      if apply_to_both_rules_values then
        let value' = t_self.GetProperty("GetImmediateValue").GetGetMethod()
        do k := fun world self dt ->
                  k' world self dt
                  let f = value_get.Invoke(self,[||])
                  let f' = value'.Invoke(self, [||])
                  do k_aux world f dt
                  do k_aux world f' dt
      else
        do k := fun world self dt ->
                  k' world self dt
                  let f = value_get.Invoke(self,[||])
                  do k_aux world f dt

    | _  when t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) ->
      do a  t_world t_self k
    | _  when t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaWorld>) ->
  //    if types_explored |> Seq.exists ((=) t_self) then
  //      failwithf "Entity %s already encountered; this means the game world contains a circular reference: %A" t_self.Name [for t in types_explored |> List.rev do yield t.Name]
      do a t_self t_self k
      let fields = t_self.GetProperties()
      for field in fields do
        if field.GetCustomAttributes(false) |> Seq.exists (fun a -> a.GetType() = typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>) then
          let f_type = field.PropertyType
          let f_attr = f_type.GetCustomAttributes(true)
          let f_get = field.GetGetMethod()

          let k_aux = ref (fun w s dt -> ())
          do traverse_entity t_self f_type apply_to_both_rules_values k_aux a type_predicate
          let k_aux = !k_aux
          let k' = !k
          do k := fun world self dt ->
                    k' self self dt
                    let f = f_get.Invoke(self, [||])
                    do k_aux self f dt
    | _  when t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaEntity>) ->
  //    if types_explored |> Seq.exists ((=) t_self) then
  //      failwithf "Entity %s already encountered; this means the game world contains a circular reference: %A" t_self.Name [for t in types_explored |> List.rev do yield t.Name]
      do a t_world t_self k
      let fields = t_self.GetProperties()
      for field in fields do
        if field.GetCustomAttributes(false) |> Seq.exists (fun a -> a.GetType() = typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>) then
          let f_type = field.PropertyType
          let f_attr = f_type.GetCustomAttributes(true)
          let f_get = field.GetGetMethod()

          let k_aux = ref (fun w s dt -> ())
          do traverse_entity t_world f_type apply_to_both_rules_values k_aux a type_predicate
          let k_aux = !k_aux
          let k' = !k
          do k := fun world self dt ->
                    k' world self dt
                    let f = f_get.Invoke(self, [||])
                    do k_aux world f dt
    | _ -> ()


let private make_delegate (m:MethodInfo) =
  if m.ReturnType.Name = "Void" then
    let m_args = [ for p in m.GetParameters() do yield p.ParameterType ]
    let m_type = System.Linq.Expressions.Expression.GetActionType(m_args |> Seq.toArray)
    in System.Delegate.CreateDelegate(m_type, m) 
  else
    let m_args = [ for p in m.GetParameters() do yield p.ParameterType
                   yield m.ReturnType ]
    let m_type = System.Linq.Expressions.Expression.GetFuncType(m_args |> Seq.toArray)
    in System.Delegate.CreateDelegate(m_type, m) 
  

let rec private update_world_entities world_type ai_tick_interval input_tick_interval () = 
  let apply_rules_with_reflection apply_rules (frequency:UpdateFrequency) (world_type:System.Type) (self_type:System.Type) k =
    let t_a = self_type
    let methods = t_a.GetMethods() |> Seq.filter (fun m -> m.Name.EndsWith("Rule") || m.Name.EndsWith("'"))
    let fields = t_a.GetProperties()
    for m in methods do
      if is_frequency_ok (m.GetCustomAttributes(false)) frequency then
        // try to apply m (a rule method) to field f
        // f = m()
        match fields |> Seq.tryFind (fun f -> f.Name + "Rule" = m.Name || f.Name + "'" = m.Name) with
        | Some field ->
          let f_get = field.GetGetMethod()
          let apply = field.PropertyType.GetMethod("Apply")
          let k' = !k
          let m_delegate = make_delegate m
          let apply_delegate = make_delegate apply
          let swap = field.PropertyType.GetMethod("Swap")
          let swap_delegate = make_delegate swap

          if f_get.ReturnType.GetGenericTypeDefinition() = typeof<Rule<_>>.GetGenericTypeDefinition() then
            if m.ReturnType <> f_get.ReturnType.GetGenericArguments().[0] then
               failwithf "Rule %s does not return a value of the expected type %s. Instead, it return %s" m.Name (f_get.ReturnType.GetGenericArguments().[0].Name) m.ReturnType.Name
//          elif f_get.ReturnType.GetGenericTypeDefinition() = typeof<RuleMap<_, _>>.GetGenericTypeDefinition() then
//            let f_key_type = f_get.ReturnType.GetGenericArguments().[0]
//            let f_value_type = f_get.ReturnType.GetGenericArguments().[1]
//            let m_key_type = m.ReturnType.GetGenericArguments().[0].GetGenericArguments().[0]
//            let m_value_type = m.ReturnType.GetGenericArguments().[0].GetGenericArguments().[1]
//            if f_key_type <> m_key_type || f_value_type <> m_value_type then
//               failwithf "Rule %s does not return a value of the expected type %s. Instead, it return %s" m.Name (f_get.ReturnType.GetGenericArguments().[0].Name) m.ReturnType.Name
          elif f_get.ReturnType.GetGenericTypeDefinition() = typeof<RuleTable<_>>.GetGenericTypeDefinition() then
            if m.ReturnType.GetGenericArguments().[0] <> f_get.ReturnType.GetGenericArguments().[0] then
               failwithf "Rule %s does not return a value of the expected type %s. Instead, it return %s" m.Name (f_get.ReturnType.GetGenericArguments().[0].Name) m.ReturnType.Name

          let m_parameters = m.GetParameters() |> Array.map (fun pi -> pi.ParameterType)
          if apply_rules |> not then
            k := fun (world:'world) (self:obj) (dt:obj) ->
                    k' world self dt
                    let f = f_get.Invoke(self, [||])
                    swap_delegate.DynamicInvoke([|f|]) |> ignore
          elif m_parameters = [| world_type; typeof<float32> |] then
            k := fun (world:'world) (self:obj) (dt:obj) ->
                    k' world self dt
                    let f' = m_delegate.DynamicInvoke([| world :> obj; dt |])
                    let f = f_get.Invoke(self, [||])
                    apply_delegate.DynamicInvoke([|f; f'|]) |> ignore
          elif m_parameters = [| self_type; typeof<float32> |] then
            k := fun (world:'world) (self:obj) (dt:obj) ->
                    k' world self dt
                    let f' = m_delegate.DynamicInvoke([| self; dt |])
                    let f = f_get.Invoke(self, [||])
                    apply_delegate.DynamicInvoke([|f; f'|]) |> ignore
          elif m_parameters = [| world_type; self_type; typeof<float32> |] then
            k := fun (world:'world) (self:obj) (dt:obj) ->
                    k' world self dt
                    let f1 = m_delegate.DynamicInvoke([| world :> obj; self; dt |])
                    let f = f_get.Invoke(self, [||])
                    apply_delegate.DynamicInvoke([|f; f1|]) |> ignore
          elif m_parameters = [| world_type; self_type |] then
            k := fun (world:'world) (self:obj) (dt:obj) ->
                    k' world self dt
                    let f' = m_delegate.DynamicInvoke([| world :> obj; self |])
                    let f = f_get.Invoke(self, [||])
                    apply_delegate.DynamicInvoke([|f; f'|]) |> ignore
          elif m_parameters = [| world_type |] then
            k := fun (world:'world) (self:obj) (dt:obj) ->
                    k' world self dt
                    let f' = m_delegate.DynamicInvoke([| world :> obj |])
                    let f = f_get.Invoke(self, [||])
                    apply_delegate.DynamicInvoke([|f; f'|]) |> ignore
          elif m_parameters = [| self_type |] then
            k := fun (world:'world) (self:obj) (dt:obj) ->
                    k' world self dt
                    let f' = m_delegate.DynamicInvoke([| self |])
                    let f = f_get.Invoke(self, [||])
                    apply_delegate.DynamicInvoke([|f; f'|]) |> ignore
          else
             failwithf "Rule %s does not take the correct parameters. Instead, it takes %A" m.Name m_parameters

        | None ->
          let mutable found = false
          // try to apply m (a rule method) to some subfield f1 of field f
          // f.f1 = m()
          for field in fields do
            let field_type = field.PropertyType
            let fields1 = field_type.GetProperties()
            match fields1 |> Seq.tryFind (fun f1 -> field.Name + f1.Name + "Rule" = m.Name || field.Name + f1.Name + "'" = m.Name) with
            | Some field1 ->
              do found <- true
              let f_get = field.GetGetMethod()
              let f1_get = field1.GetGetMethod()
              let apply = field1.PropertyType.GetMethod("Apply")
              let m_delegate = make_delegate m
              let apply_delegate = make_delegate apply
              let swap = field1.PropertyType.GetMethod("Swap")
              let swap_delegate = make_delegate swap
              let k' = !k
              if apply_rules then
                if m.GetParameters().Length = 3 then
                  k := fun (world:'world) (self:obj) (dt:obj) ->
                          let m = m
                          k' world self dt
                          let f' = m_delegate.DynamicInvoke([| world :> obj; self; dt |])
                          let f = f_get.Invoke(self, [||])
                          let f1 = f1_get.Invoke(f, [||])
                          apply_delegate.DynamicInvoke([|f1; f'|]) |> ignore
                else
                  k := fun (world:'world) (self:obj) (dt:obj) ->
                          k' world self dt
                          let f' = m_delegate.DynamicInvoke([| self; dt |])
                          let f = f_get.Invoke(self, [||])
                          let f1 = f1_get.Invoke(f, [||])
                          apply_delegate.DynamicInvoke([|f1; f'|]) |> ignore
              else
                k := fun (world:'world) (self:obj) (dt:obj) ->
                        k' world self dt
                        let f = f_get.Invoke(self, [||])
                        let f1 = f1_get.Invoke(f, [||])
                        swap_delegate.DynamicInvoke([|f1|]) |> ignore
            | None -> 
              // try to apply m (a rule method) to some subfield f2 of field f1
              // f.f1.f2 = m()
              for field1 in fields1 do
                let field1_type = field1.PropertyType
                let fields2 = field1_type.GetProperties()
                
                match fields2 |> Seq.tryFind (fun f2 -> field.Name + field1.Name + f2.Name + "Rule" = m.Name || field.Name + field1.Name + f2.Name + "'" = m.Name) with
                | Some field2 ->                                    
                  let f_get = field.GetGetMethod()
                  let f1_get = field1.GetGetMethod()
                  let f2_get = field2.GetGetMethod()
                  let apply = field2.PropertyType.GetMethod("Apply")
                  let m_delegate = make_delegate m
                  let apply_delegate = make_delegate apply
                  let swap = field2.PropertyType.GetMethod("Swap")
                  let swap_delegate = make_delegate swap
                  let k' = !k
                  if apply_rules then
                    if m.GetParameters().Length = 3 then
                      k := fun (world:'world) (self:obj) (dt:obj) ->
                              let m = m
                              k' world self dt
                              let f' = m_delegate.DynamicInvoke([| world :> obj; self; dt |])
                              let f = f_get.Invoke(self, [||])
                              let f1 = f1_get.Invoke(f, [||])
                              let f2 = f2_get.Invoke(f1, [||])
                              apply_delegate.DynamicInvoke([|f2; f'|]) |> ignore
                    else
                      k := fun (world:'world) (self:obj) (dt:obj) ->
                              k' world self dt
                              let f' = m_delegate.DynamicInvoke([| self; dt |])
                              let f = f_get.Invoke(self, [||])
                              let f1 = f1_get.Invoke(f, [||])
                              let f2 = f2_get.Invoke(f1, [||])
                              apply_delegate.DynamicInvoke([|f2; f'|]) |> ignore
                  else
                    k := fun (world:'world) (self:obj) (dt:obj) ->
                            k' world self dt
                            let f = f_get.Invoke(self, [||])
                            let f1 = f1_get.Invoke(f, [||])
                            let f2 = f2_get.Invoke(f1, [||])
                            swap_delegate.DynamicInvoke([|f2|]) |> ignore
                | None -> ()

  types_with_draw.Clear()
  types_with_realtime_rules.Clear()
  types_with_ai_rules.Clear()
  types_with_interactive_rules.Clear()

  do pre_traverse_entity true UpdateFrequency.RealTime types_with_realtime_rules [] world_type
  do pre_traverse_entity false UpdateFrequency.AI types_with_ai_rules [] world_type
  do pre_traverse_entity false UpdateFrequency.Interactive types_with_interactive_rules [] world_type

  let ai_update = ref (fun world self dt -> ())
  do traverse_entity world_type world_type false ai_update (apply_rules_with_reflection true UpdateFrequency.AI) (fun t -> types_with_ai_rules.[t])
  let ai_update = !ai_update
  let ai_swap = ref (fun world self dt -> ())
  do traverse_entity world_type world_type false ai_swap (apply_rules_with_reflection false UpdateFrequency.AI) (fun t -> types_with_ai_rules.[t])
  let ai_swap = !ai_swap

  let interactive_update = ref (fun world self dt -> ())
  do traverse_entity world_type world_type false interactive_update (apply_rules_with_reflection true UpdateFrequency.Interactive) (fun t -> types_with_interactive_rules.[t])
  let interactive_update = !interactive_update
  let interactive_swap = ref (fun world self dt -> ())
  do traverse_entity world_type world_type false interactive_swap (apply_rules_with_reflection false UpdateFrequency.Interactive) (fun t -> types_with_interactive_rules.[t])
  let interactive_swap = !interactive_swap

  let inline timer t max_t dt a not_a = 
    if !t >= max_t then
      do t := !t - max_t
      do a()
    else
      do not_a()
    do t := !t + dt

  let ai_tick_timer = ref 0.0f
  let input_tick_timer = ref 0.0f
  let update world self (dt:obj) = 
    let dt = dt :?> float32
    let dt = dt / time_speed
    do timer ai_tick_timer ai_tick_interval dt 
              (fun () -> ai_update world self (ai_tick_interval * time_speed))
              (fun () -> ai_swap world self (ai_tick_interval * time_speed))
    do timer input_tick_timer input_tick_interval dt 
              (fun () -> interactive_update world self (input_tick_interval * time_speed))
              (fun () -> interactive_swap world self (input_tick_interval * time_speed))
  let update = ref update
  do traverse_entity world_type world_type false update (apply_rules_with_reflection true UpdateFrequency.RealTime) (fun t -> types_with_realtime_rules.[t])
  let update = !update
  fun world dt -> update world (world:>obj) (dt:>obj)


let private update_script (main:Var<_>) = main.Value <- Coroutines.update_script main.Value


let internal update_world ai_tick_interval input_tick_interval (world:'world) =
  let update_world_entities = update_world_entities (world.GetType()) ai_tick_interval input_tick_interval ()
  let update_index = ref 0
  let ai_tick_timer = ref 0.0f
  let input_tick_timer = ref 0.0f
  fun main (dt:float32) ->
    do update_world_entities world dt
    let dt = dt / time_speed
    do update_script main
    do step_scripts()
    do timer ai_tick_timer ai_tick_interval dt step_ai_scripts
    do timer input_tick_timer input_tick_interval dt step_input_scripts
    do commit_rule_updates()
    do incr update_index

let internal draw_world (default_layer : SpriteLayer) world = 
  let clear_and_register_drawables (world_type:System.Type) (self_type:System.Type) k =
    let t_a = self_type
    if self_type.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_clear = t_a.GetMethod("Clear")
      let t_register = t_a.GetMethod("Register")     
      let k' = !k
      do k := fun (world:'world) (self:obj) () ->
                k' world self ()
                do t_clear.Invoke(self, [||]) |> ignore
                do t_register.Invoke(self, [||]) |> ignore

  let draw_drawables (world_type:System.Type) (self_type:System.Type) k =
    let t_a = self_type
    if t_a.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_methods = t_a.GetMethods()
      match t_methods |> Seq.tryFind (fun m -> m.Name = "Draw") with
      | Some t_draw -> 
        let k' = !k
        do k := fun (world:'world) (self:obj) () ->
                  k' world self ()
                  do t_draw.Invoke(self, [||]) |> ignore
      | None -> ()

  types_with_draw.Clear()
  types_with_realtime_rules.Clear()
  do pre_traverse_entity true UpdateFrequency.RealTime types_with_realtime_rules [] (world.GetType())

  let clear = ref (fun world self () -> ())
  do traverse_entity (world.GetType()) (world.GetType()) false clear clear_and_register_drawables
                     (fun t -> types_with_draw.[t])
  let clear = !clear

  let draw = ref (fun world self () -> ())
  do traverse_entity (world.GetType()) (world.GetType()) false draw draw_drawables (fun t -> types_with_draw.[t])
  let draw = !draw

  fun () ->    
    default_layer.Clear()
    default_layer.Register()
    clear world (world:>obj) ()
    
    default_layer.Draw()
    draw world (world:>obj) ()
    
let internal reload_drawables<'world> world = 
  let reload_drawable (world_type:System.Type) (self_type:System.Type) k =
    let t_a = self_type
    if self_type.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_methods = t_a.GetMethods()
      match t_methods |> Seq.tryFind (fun m -> m.Name = "Reload") with
      | Some t_reload when t_reload.GetParameters().Length = 0 -> 
        let k' = !k
        do k := fun (world:obj) (self:obj) () ->
                  k' world self ()
                  do t_reload.Invoke(self, [||]) |> ignore
      | _ -> ()
      
  types_with_draw.Clear()
  types_with_realtime_rules.Clear()
  do pre_traverse_entity true UpdateFrequency.RealTime types_with_realtime_rules [] (world.GetType())
  
  let reload = ref (fun world self () -> ())
  do traverse_entity (world.GetType()) (world.GetType()) true reload reload_drawable (fun t -> types_with_draw.[t])
  let reload = !reload
  do reload world (world:>obj) ()

let internal reload_layers<'world> world (content : ContentManager) (graphics_device : GraphicsDevice) = 
  let reload_layers (world_type:System.Type) (self_type:System.Type) k =
    let t_a = self_type
    if self_type.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_methods = t_a.GetMethods()
      match t_methods |> Seq.tryFind (fun m -> m.Name = "Reload") with
      | Some t_reload when t_reload.GetParameters().Length > 0 -> 
        let k' = !k
        do k := fun (world:obj) (self:obj) () ->
                  k' world self ()
                  do t_reload.Invoke(self, [|content; graphics_device|]) |> ignore
      | _ -> ()

  types_with_draw.Clear()
  types_with_realtime_rules.Clear()
  do pre_traverse_entity true UpdateFrequency.RealTime types_with_realtime_rules [] (world.GetType())
  
  let reload = ref (fun world self () -> ())
  do traverse_entity (world.GetType()) (world.GetType()) true reload reload_layers (fun t -> types_with_draw.[t])
  let reload = !reload
  do reload world (world:>obj) ()

//let internal update_and_draw_world_async (world:'world) (default_layer : SpriteLayer) =
//  let update_world_entities = update_world_entities ()
//  let draw_world = draw_world default_layer
//  let dt_ref = ref 0.001f
//  let update_async = 
//    async{
//      return update_world_entities world !dt_ref
//    }
//  let draw_async = 
//    async{
//      return draw_world world ()
//    }
//  let tick_async world = Async.Parallel [update_async; draw_async] |> Async.Ignore
//  fun main (dt:float32) ->
//    do dt_ref := dt
//    do tick_async world |> Async.RunSynchronously
//    do update_script main
//    do step_scripts()
//    do commit_rule_updates()
