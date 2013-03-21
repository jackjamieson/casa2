module Casanova.SlowStateTraversal

open Casanova.Core
open Casanova.Coroutines
open System
open TypePredicates
open Casanova.Drawing
open Microsoft.FSharp.Reflection

let rec private traverse_entity (t_self : Type) (world:'world) (self:obj) (dt:'a) (a : 'world -> obj -> 'a -> Unit) =   
  match t_self with

  | RuleTableType(list_arg) -> 
    for x in self :?> System.Collections.IEnumerable do
      do traverse_entity list_arg world x dt a
  
  | ListType(list_arg) -> 
    for x in self :?> System.Collections.IEnumerable do
      do traverse_entity list_arg world x dt a
  
  | OptionType(option_arg:Type) ->
    let f_methods = t_self.GetMethods()
    let isSome_prop = t_self.GetProperty("IsSome")                
    let isSome_prop_get = isSome_prop.GetGetMethod()
    let parameters = isSome_prop_get.GetParameters()
    if (isSome_prop_get.Invoke(null,[|self|]) :?> bool) = true then
      let option_value = t_self.GetProperty("Value")                
      let option_value_get = option_value.GetGetMethod()
      let f = option_value_get.Invoke(self,[||])
      do traverse_entity option_arg world f dt a
  | VarType(arg) | RuleType(arg) ->
    let value = t_self.GetProperty("Value")                
    let value_get = value.GetGetMethod()
    let f = value_get.Invoke(self,[||])
    do traverse_entity arg world f dt a
  | RefType(arg) -> ()
  | UnionType(cases) ->
    let case, parameters = FSharpValue.GetUnionFields(self, t_self)
    for parameter in parameters do
      if parameter <> null then
        do traverse_entity (parameter.GetType()) world parameter dt a
  | _  when t_self.GetCustomAttributes(false) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) ->
    do a world self dt
  | _  when t_self.GetCustomAttributes(false) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaEntity>) ->
    do a world self dt
    let fields = t_self.GetProperties()
    for field in fields do
      if field.GetCustomAttributes(false) |> Seq.exists (fun a -> a.GetType() = typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>) then
        let f_type = field.PropertyType
        let f_get = field.GetGetMethod()
        let f = f_get.Invoke(self, [||])
        do traverse_entity f_type world f dt a
//      else
//        printf "Guby pliz"
  | _ -> ()

let rec private update_world_entities (world:'world) (dt:float32) = 
  let apply_rules_with_reflection (world:'world) (self:obj) (dt:float32) =
    let t_a = self.GetType()
    if t_a.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaEntity>) then
      let methods = t_a.GetMethods() |> Seq.filter (fun m -> m.Name.EndsWith("Rule"))
      let fields = t_a.GetProperties()
      for m in methods do
        // try to apply m (a rule method) to field f
        // f = m()
        match fields |> Seq.tryFind (fun f -> f.Name + "Rule" = m.Name) with
        | Some field ->
          let f_get = field.GetGetMethod()
          let f = f_get.Invoke(self, [||])
          let apply = f.GetType().GetMethod("Apply")
          let f' = 
            if m.GetParameters().Length = 3 then
              m.Invoke(null, [| world; self; dt |])
            else
              m.Invoke(null, [| self; dt |])
          apply.Invoke(null, [|f; f'|]) |> ignore
        | None ->
          // try to apply m (a rule method) to some subfield f1 of field f
          // f.f1 = m()
          for field in fields do
            let field_type = field.PropertyType
            let fields1 = field_type.GetProperties()
            match fields1 |> Seq.tryFind (fun f1 -> field.Name + f1.Name + "Rule" = m.Name) with
            | Some field1 ->
              let f_get = field.GetGetMethod()
              let f = f_get.Invoke(self, [||])
              let f1_get = field1.GetGetMethod()
              let f1 = f1_get.Invoke(f, [||])
              let apply = f1.GetType().GetMethod("Apply")
              let f' = 
                if m.GetParameters().Length = 3 then
                  m.Invoke(null, [| world; self; dt |])
                else
                  m.Invoke(null, [| self; dt |])
              apply.Invoke(null, [|f1; f'|]) |> ignore
            | None -> ()     

  do traverse_entity typeof<'world> world (world :> obj) dt apply_rules_with_reflection


let private update_script (main:Var<_>) = main.Value <- Coroutines.update_script main.Value


let internal update_world (world:'world) main (dt:float32) =
  do update_world_entities world dt
  do update_script main
  do step_scripts()
  do commit_rule_updates()


let internal draw_world (default_layer : SpriteLayer) (world:'world) () = 
  let clear_and_register_drawables (world:'world) (self:obj) () =
    let t_a = self.GetType()
    if t_a.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_clear = t_a.GetMethod("Clear")        
      do t_clear.Invoke(self, [||]) |> ignore
      let t_clear = t_a.GetMethod("Register")
      do t_clear.Invoke(self, [||]) |> ignore

  let draw_drawables (world:'world) (self:obj) () =
    let t_a = self.GetType()
    if t_a.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_methods = t_a.GetMethods()
      match t_methods |> Seq.tryFind (fun m -> m.Name = "Draw") with
      | Some t_draw -> do t_draw.Invoke(self, [||]) |> ignore
      | None -> ()

  do default_layer.Clear()
  do default_layer.Register()
  do traverse_entity typeof<'world> world (world :> obj) () clear_and_register_drawables

  do default_layer.Draw()
  do traverse_entity typeof<'world> world (world :> obj) () draw_drawables
