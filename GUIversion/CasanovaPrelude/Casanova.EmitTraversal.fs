module Casanova.EmitTraversal

open Casanova.Core
open Casanova.Utilities
open Casanova.Coroutines
open System
open Casanova.Action
open System.Reflection
open System.Reflection.Emit
open TypePredicates
open Casanova.Drawing
open Casanova.Action
open Microsoft.FSharp.Reflection
open Casanova.PreTraverse
open AccelerationIndices
open Casanova.Math

type internal ConditionFromType =
  | FromMethod of MethodInfo
  | FromFloat32 of float32
  | FromInt32 of int
  | FromBool of int

type internal RuleOperation = Apply | Swap
//let mutable internal program : Type = null
//let mutable internal assemblybuilder = null
//let mutable internal modulebuilder =  null
//let mutable internal programmclass = null
//let mutable internal cons = null
//let mutable all_resize_arrays = []

let rec private traverse_entity (apply_to_both_rule_values:bool) (k:ILGenerator) (get_local_variable:ILGenerator->Type->LocalBuilder) a (type_predicate:System.Type -> bool) (get_world:ILGenerator->Unit) (t_world : System.Type) (t_self : System.Type) = 
  let traverse_entity = traverse_entity apply_to_both_rule_values k get_local_variable a type_predicate
  let a = a get_local_variable
  let get_local_variable = get_local_variable k
  if type_predicate t_self then
    // args = world,dt
    // S_in = S;self
    // S_out = S;self
    match t_self with
    | RefType(arg) -> ()

    | Tuple(args) ->
      for i,arg in args |> Seq.mapi (fun i a -> i,a) do
        do k.Emit(OpCodes.Dup) // S = t;t
        let get_arg = t_self.GetMethod("get_Item" + (i+1).ToString())
        do k.EmitCall(OpCodes.Call, get_arg, null) // S = t;i
        do traverse_entity get_world t_world arg
        do k.Emit(OpCodes.Pop) // S = t

  
    | RuleTableType(list_arg) -> 
        let i = get_local_variable typeof<int>
        do k.Emit(OpCodes.Ldc_I4, 0) // S = r;0
        do k.Emit(OpCodes.Stloc, i)  // S = r
        let get_Value = t_self.GetMethod("GetValueMember")
        let get_Count = typedefof<ResizeArray<_>>.MakeGenericType([|list_arg|]).GetMethod("get_Count")
        let get_Item = typedefof<ResizeArray<_>>.MakeGenericType([|list_arg|]).GetMethod("get_Item")
        do k.Emit(OpCodes.Dup)  // S = r;r
        do k.EmitCall(OpCodes.Call, get_Value, null) // S = r;v
        let traverse_rule_table() =
          let loop = k.DefineLabel()
          let end_loop = k.DefineLabel()
          do k.MarkLabel(loop)
          do k.Emit(OpCodes.Dup) // S = r;v;v
          do k.EmitCall(OpCodes.Call, get_Count, null) // S = r;v;vl
          do k.Emit(OpCodes.Ldloc, i) // S = r;v;vl;i
          do k.Emit(OpCodes.Beq, end_loop) // S = r;v
          do k.Emit(OpCodes.Dup) // S = r;v;v
          do k.Emit(OpCodes.Ldloc, i) // S = r;v;v;i
          do k.EmitCall(OpCodes.Call, get_Item, null) // S = r;v;x
          do traverse_entity get_world t_world list_arg // S = r;v;x
          do k.Emit(OpCodes.Pop) // S = r;v
          do k.Emit(OpCodes.Ldloc, i) // S = r;v;i
          do k.Emit(OpCodes.Ldc_I4, 1) // S = r;v;i;1
          do k.Emit(OpCodes.Add) // S = r;v;i
          do k.Emit(OpCodes.Stloc, i)  // S = r;v
          do k.Emit(OpCodes.Br, loop)
          do k.MarkLabel(end_loop)
          do k.Emit(OpCodes.Pop) // S = r
        do traverse_rule_table()
        if apply_to_both_rule_values then
          do k.Emit(OpCodes.Ldc_I4, 0) // S = r;0
          do k.Emit(OpCodes.Stloc, i)  // S = r
          let get_Value = t_self.GetMethod("GetImmediateValueMember")
          do k.Emit(OpCodes.Dup)  // S = r;r
          do k.EmitCall(OpCodes.Call, get_Value, null) // S = r;v
          do traverse_rule_table()
    | ArrayType(list_arg) ->
        let i = get_local_variable typeof<int>
        do k.Emit(OpCodes.Ldc_I4, 0) // S = r;0
        do k.Emit(OpCodes.Stloc, i)  // S = r
        let get_Count = t_self.GetMethod("get_Length")
        let get_Item = t_self.GetMethod("Get")
        do k.Emit(OpCodes.Dup)  // S = r;r
        let loop = k.DefineLabel()
        let end_loop = k.DefineLabel()
        do k.MarkLabel(loop)
        do k.Emit(OpCodes.Dup) // S = r;v;v
        do k.EmitCall(OpCodes.Call, get_Count, null) // S = r;v;vl
        do k.Emit(OpCodes.Ldloc, i) // S = r;v;vl;i
        do k.Emit(OpCodes.Beq, end_loop) // S = r;v
        do k.Emit(OpCodes.Dup) // S = r;v;v
        do k.Emit(OpCodes.Ldloc, i) // S = r;v;v;i
        do k.EmitCall(OpCodes.Call, get_Item, null) // S = r;v;x
        do traverse_entity get_world t_world list_arg // S = r;v;x
        do k.Emit(OpCodes.Pop) // S = r;v
        do k.Emit(OpCodes.Ldloc, i) // S = r;v;i
        do k.Emit(OpCodes.Ldc_I4, 1) // S = r;v;i;1
        do k.Emit(OpCodes.Add) // S = r;v;i
        do k.Emit(OpCodes.Stloc, i)  // S = r;v
        do k.Emit(OpCodes.Br, loop)
        do k.MarkLabel(end_loop)
        do k.Emit(OpCodes.Pop) // S = r
    | ListType(list_arg) -> 
        let get_tail = t_self.GetMethod("get_Tail")
        let get_head = t_self.GetMethod("get_Head")
        let is_empty = t_self.GetMethod("get_IsEmpty")
        let loop = k.DefineLabel()
        let end_loop = k.DefineLabel()
        do k.Emit(OpCodes.Dup)
        do k.MarkLabel(loop)
        do k.Emit(OpCodes.Dup) // S = l0;l;l
        do k.EmitCall(OpCodes.Call, is_empty, null) // S = l0;l;b
        do k.Emit(OpCodes.Brtrue, end_loop) // S = l0;l
        do k.Emit(OpCodes.Dup) // S = l0;l;l
        do k.EmitCall(OpCodes.Call, get_head, null) // S = l0;l;x
        do traverse_entity get_world t_world list_arg // S = l0;l;x
        do k.Emit(OpCodes.Pop) // S = l0;l
        do k.EmitCall(OpCodes.Call, get_tail, null) // S = l0;l.Tail
        do k.Emit(OpCodes.Br, loop)
        do k.MarkLabel(end_loop)
        do k.Emit(OpCodes.Pop) // S = l0

    | OptionType(option_arg) ->
      let isSome = t_self.GetMethod("get_IsSome")
      let get_Value = t_self.GetMethod("get_Value")
      let skip = k.DefineLabel()
      do k.Emit(OpCodes.Dup) // S = o;o
      do k.EmitCall(OpCodes.Call, isSome, null)
      do k.Emit(OpCodes.Brfalse, skip) // S = o
      do k.Emit(OpCodes.Dup) // S = o;o
      do k.EmitCall(OpCodes.Call, get_Value, null)
      do traverse_entity get_world t_world option_arg
      do k.Emit(OpCodes.Pop)
      do k.MarkLabel(skip)
      do k.Emit(OpCodes.Nop)
      
    | UnionType(cases) ->
      let case_types =
        [
          for case in cases do
            yield t_self.GetNestedType(case.Name)
        ]
      let end_cases = k.DefineLabel()
      for case in case_types do
        do k.Emit(OpCodes.Dup) // u;u
        do k.Emit(OpCodes.Isinst, case) // u;c
        do k.Emit(OpCodes.Dup) // u;c;c
        do k.Emit(OpCodes.Ldnull) // u;c;c;0
        let next_case = k.DefineLabel()
        do k.Emit(OpCodes.Beq, next_case) // u;c
        do traverse_entity get_world t_world case
        do k.Emit(OpCodes.Jmp, end_cases)
        do k.MarkLabel(next_case)
        do k.Emit(OpCodes.Nop)
      do k.MarkLabel(end_cases)
      do k.Emit(OpCodes.Pop) // u


    | VarType(arg) ->
      do k.Emit(OpCodes.Dup) // S = v;v
      let get_Value = t_self.GetMethod("get_Value")
      do k.EmitCall(OpCodes.Call, get_Value, null) // S = v;x
      do traverse_entity get_world t_world arg
      do k.Emit(OpCodes.Pop) // S = v

    | RuleType(arg) ->
      let get_Value = t_self.GetMethod("GetValueMember")
      do k.EmitCall(OpCodes.Call, get_Value, null)
      do traverse_entity get_world t_world arg
      if apply_to_both_rule_values then
        let get_Value = t_self.GetMethod("GetImmediateValueMember")
        do k.EmitCall(OpCodes.Call, get_Value, null)
        do traverse_entity get_world t_world arg
    | _  when t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) ->
      do a t_self k get_world t_world
    | _  when t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaEntity>) ->
      do k.Emit(OpCodes.Dup)
      do a t_self k get_world t_world
      let fields = t_self.GetProperties()
      for field in fields do
        if field.GetCustomAttributes(false) |> Seq.exists (fun a -> a.GetType() = typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>) then
          let field_type = field.PropertyType
          let get_Field = field.GetGetMethod() 
          do k.Emit(OpCodes.Dup)
          do k.EmitCall(OpCodes.Call, get_Field, null)
          do traverse_entity get_world t_world field_type
          do k.Emit(OpCodes.Pop)
      do k.Emit(OpCodes.Pop)
    | _  when t_self.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaWorld>) ->
      do k.Emit(OpCodes.Dup)
      let new_world = get_local_variable t_self
      do k.Emit(OpCodes.Dup)
      do k.Emit(OpCodes.Stloc, new_world)
      let get_world (k:ILGenerator) = k.Emit(OpCodes.Ldloc, new_world)
      let t_world = t_self
      do a t_self k get_world t_world
      let fields = t_self.GetProperties()
      for field in fields do
        if field.GetCustomAttributes(false) |> Seq.exists (fun a -> a.GetType() = typeof<Microsoft.FSharp.Core.CompilationMappingAttribute>) then
          let field_type = field.PropertyType
          let get_Field = field.GetGetMethod()
          do k.Emit(OpCodes.Dup)
          do k.EmitCall(OpCodes.Call, get_Field, null)
          do traverse_entity get_world t_world field_type
          do k.Emit(OpCodes.Pop)
      do k.Emit(OpCodes.Pop)
    | _ -> ()

let private incremental_invocation = var 0
let update_and_draw<'world> ai_tick_interval input_tick_interval (default_layer:SpriteLayer) (world:'world) (main:Var<Script>) =
  do reset_type_dictionaries()
  let t_world = typeof<'world>

  do pre_traverse_entity true UpdateFrequency.RealTime types_with_realtime_rules [] t_world
  do pre_traverse_entity false UpdateFrequency.AI types_with_ai_rules [] t_world
  do pre_traverse_entity false UpdateFrequency.Interactive types_with_interactive_rules [] t_world
  
  let action_targets, types_which_do_or_recieve_actions =
    let types_which_do_or_recieve_actions = System.Collections.Generic.HashSet<System.Type>()
    let all_types = all_types
    let dictionary = System.Collections.Generic.Dictionary<System.Type * string * ActionAttribute, System.Type list> ()
    let types_with_actions = 
      [for key_value_pair in types_with_actions do
        if key_value_pair.Value then           
          yield key_value_pair.Key]
    for type_with_actions in types_with_actions do
      for field in type_with_actions.GetProperties() do
        
        if field.PropertyType.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<ActionAttribute>) then        
          let action = field.PropertyType.GetCustomAttributes(true) |> Seq.find (fun a -> a.GetType() = typeof<ActionAttribute>) :?> ActionAttribute
          let targets = action.Target.Split(';') |> Array.map (fun s -> s.Trim())
          for target in targets do
            let actions_targets_lst = 
              if dictionary.ContainsKey(type_with_actions, field.Name, action) then                 
                let res = dictionary.[type_with_actions, field.Name, action] 
                dictionary.Remove(type_with_actions, field.Name, action) |> ignore
                res else []
            let prev_type = 
              if target = "Self" then type_with_actions
              else all_types.[target] 
            dictionary.Add((type_with_actions, field.Name, action),  prev_type :: actions_targets_lst)            
    for source_type_to_targets in dictionary do
      let source_type,_,_ = source_type_to_targets.Key
      types_which_do_or_recieve_actions.Add(source_type) |> ignore
      for target_types in source_type_to_targets.Value do
        types_which_do_or_recieve_actions.Add(target_types) |> ignore    



    dictionary, types_which_do_or_recieve_actions

  let update_RT,update_AI,update_IN,swap_RT,swap_AI,swap_IN,pre_draw,draw,reload_drawables,reload_layers,assemblybuilder,programmclass,name, suffix, static_fields_dictionary, apply_actions, static_indices_fields_dictionary,types_and_radius, targets_container =
      let suffix = string(!incremental_invocation)
      do incremental_invocation := !incremental_invocation

      let name = "update_and_draw"

      let assemblyname = new AssemblyName(name)
      let name = name + ".dll"
      let assemblybuilder = AppDomain.CurrentDomain.DefineDynamicAssembly(assemblyname, AssemblyBuilderAccess.RunAndSave)
      let modulebuilder = assemblybuilder.DefineDynamicModule(name)
      let programmclass = modulebuilder.DefineType("Program",TypeAttributes.Public)
      let cons = programmclass.DefineConstructor(MethodAttributes.Public ||| MethodAttributes.Static, CallingConventions.Standard, [||])

      let stat_cons_il = cons.GetILGenerator()
      let static_fields_dictionary = System.Collections.Generic.Dictionary<Type, FieldBuilder>()
      let static_indices_fields_dictionary = System.Collections.Generic.Dictionary<Type * float32, FieldBuilder>()

      let types_and_radius =
        let types_and_radius = System.Collections.Generic.Dictionary<System.Type, System.Collections.Generic.List<float32>>()
        for action_target in action_targets do
          let source_type, field, _ = action_target.Key          
          let ``type`` = source_type
          let field = ``type``.GetProperty(field)          

          if field.PropertyType.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<RadiusAttribute>) then
            let action = field.PropertyType.GetCustomAttributes(true) |> Seq.find (fun a -> a.GetType() = typeof<RadiusAttribute>) :?> RadiusAttribute
            for target in action_target.Value do
              if types_and_radius.ContainsKey(target) then 
                types_and_radius.[target].Add(action.Radius)
              else 
                let radius_lst = System.Collections.Generic.List<float32>()
                radius_lst.Add(action.Radius)
                types_and_radius.Add(target, radius_lst)
        types_and_radius
      
      let accelerationindex_create_method = typeof<AccelerationIndex>.GetMethod("Create")      
      for type_and_radius in types_and_radius do
        let target = type_and_radius.Key
        let radius_list = type_and_radius.Value |> Set.ofSeq

        for radius in radius_list do
          let radius_string = (string radius).Replace('.','_')
          let field = programmclass.DefineField(target.Name.ToLower() + "_index_" + radius_string, typeof<AccelerationIndex>, FieldAttributes.Public ||| FieldAttributes.Static)
          do stat_cons_il.Emit(OpCodes.Ldc_R4,radius)
          do stat_cons_il.Emit(OpCodes.Ldc_R4,radius)
          do stat_cons_il.Emit(OpCodes.Ldc_I4,20)
          do stat_cons_il.Emit(OpCodes.Ldc_I4,20)
          do stat_cons_il.Emit(OpCodes.Ldc_I4,50)
          do stat_cons_il.EmitCall(OpCodes.Call,accelerationindex_create_method, null)
          do stat_cons_il.Emit(OpCodes.Stsfld, field); // S = empty
          do static_indices_fields_dictionary.Add((target, radius), field)

      let targets_container =
        let int_list = typeof<System.Collections.Generic.List<int>>
        let field = programmclass.DefineField("targets_container_list", int_list, FieldAttributes.Public ||| FieldAttributes.Static)

        let ctor = int_list.GetConstructor(
                    BindingFlags.Instance ||| BindingFlags.Public, null,
                    CallingConventions.HasThis, [||], null);

        
        do stat_cons_il.Emit(OpCodes.Newobj,ctor) // S = lst
        do stat_cons_il.Emit(OpCodes.Stsfld, field); // S = empty
        field
        

      for _type in types_which_do_or_recieve_actions do

        
        
        let generic_list = typeof<System.Collections.Generic.List<_>>.GetGenericTypeDefinition()
        let _type_list = generic_list.MakeGenericType([|_type|])

        let field = programmclass.DefineField(_type.Name.ToLower() + "_list", _type_list, FieldAttributes.Public ||| FieldAttributes.Static)
      

        let ctor = _type_list.GetConstructor(
                    BindingFlags.Instance ||| BindingFlags.Public, null,
                    CallingConventions.HasThis, [||], null);

        
        do stat_cons_il.Emit(OpCodes.Newobj,ctor) // S = lst
        do stat_cons_il.Emit(OpCodes.Stsfld, field); // S = empty

        do static_fields_dictionary.Add(_type, field)

      stat_cons_il.Emit(OpCodes.Ret)

      let update_RT = programmclass.DefineMethod("Update_RT" + suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>;typeof<float32>|])
      let update_AI = programmclass.DefineMethod("Update_AI" + suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>;typeof<float32>|])
      let update_IN = programmclass.DefineMethod("Update_IN"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>;typeof<float32>|])
      let swap_RT = programmclass.DefineMethod("Swap_RT"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>;typeof<float32>|])
      let swap_AI = programmclass.DefineMethod("Swap_AI"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>;typeof<float32>|])
      let swap_IN = programmclass.DefineMethod("Swap_IN"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>;typeof<float32>|])
      let pre_draw = programmclass.DefineMethod("PreDraw"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>|])
      let draw = programmclass.DefineMethod("Draw"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>|])
      let reload_drawables = programmclass.DefineMethod("ReloadDrawables"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>;typeof<float32>|])      
      let reload_layers = programmclass.DefineMethod("ReloadLayers"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>;typeof<float32>;typeof<ContentManager>;typeof<GraphicsDevice>|])
      let apply_actions = programmclass.DefineMethod("ApplyActions"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<float32>|])

      update_RT, update_AI, update_IN, swap_RT, swap_AI, swap_IN, pre_draw, draw, reload_drawables, reload_layers, assemblybuilder, programmclass, name ,suffix ,static_fields_dictionary, apply_actions, static_indices_fields_dictionary,types_and_radius, targets_container 


  for update in [update_RT;update_AI;update_IN;] do
    do update.DefineParameter(1, ParameterAttributes.In, "world") |> ignore
    do update.DefineParameter(2, ParameterAttributes.In, "dt") |> ignore
  
  let apply_actions_il = apply_actions.GetILGenerator(256)
  let update_RT_il = update_RT.GetILGenerator(256)
  do update_RT_il.Emit(OpCodes.Ldarg_0)
  let update_AI_il = update_AI.GetILGenerator(256)
  do update_AI_il.Emit(OpCodes.Ldarg_0)
  let update_IN_il = update_IN.GetILGenerator(256)
  do update_IN_il.Emit(OpCodes.Ldarg_0)
  let swap_RT_il = swap_RT.GetILGenerator(256)
  do swap_RT_il.Emit(OpCodes.Ldarg_0)
  let swap_AI_il = swap_AI.GetILGenerator(256)
  do swap_AI_il.Emit(OpCodes.Ldarg_0)
  let swap_IN_il = swap_IN.GetILGenerator(256)
  do swap_IN_il.Emit(OpCodes.Ldarg_0)
  let reload_drawables_il = reload_drawables.GetILGenerator(256)
  do reload_drawables_il.Emit(OpCodes.Ldarg_0)
  let reload_layers_il = reload_layers.GetILGenerator(256)
  do reload_layers_il.Emit(OpCodes.Ldarg_0)

  let get_local_variable () = 
//    let num_locals = var 0
//    let bound_vars = new System.Collections.Generic.Dictionary<Type,int>()
    fun (k:ILGenerator) t -> k.DeclareLocal(t)
//      if bound_vars.ContainsKey t then
//        bound_vars.[t]
//      else
//        do k.DeclareLocal(t) |> ignore
//        let l = !num_locals
//        do num_locals := l + 1
//        do bound_vars.Add(t,l)
//        l
  let update_entities (rule_op:RuleOperation) (frequency:UpdateFrequency) (get_local_variable:ILGenerator->Type->LocalBuilder) (self_type:System.Type) (k:ILGenerator) (get_world:ILGenerator->Unit) (t_world:Type) =
    
    let get_local_variable = get_local_variable k
    
    match rule_op with
    | RuleOperation.Apply when types_which_do_or_recieve_actions |> Seq.exists((=) self_type) ->       
      if static_fields_dictionary.ContainsKey(self_type) then          
          let f = static_fields_dictionary.[self_type]
          let self_type_lst = typeof<System.Collections.Generic.List<_>>.GetGenericTypeDefinition().MakeGenericType([|self_type|])       
          do k.Emit(OpCodes.Dup) //S = s ; s
          let self = get_local_variable self_type
          do k.Emit(OpCodes.Stloc, self) // S = s
          if types_and_radius.ContainsKey(self_type) then
            k.Emit(OpCodes.Ldsfld, f) // S = lst
            
            let count_method = self_type_lst.GetMethod("get_Count")
            k.EmitCall(OpCodes.Call, count_method, null) //S = count
            let acceleration_index_field_add = typeof<AccelerationIndex>.GetMethod("Add")
            for radius in types_and_radius.[self_type] do
              let acceleration_index_field = static_indices_fields_dictionary.[self_type,radius]              
              do k.Emit(OpCodes.Dup)  //S= count; count
              
              let fields = self_type.GetProperties()
              match fields |> Seq.tryFind(fun p -> p.Name.ToLower() = "position") with
              | Some p ->
                match p.PropertyType with
                | RuleType(p_t) ->
                  let get_p = p.GetGetMethod()                  
                  k.Emit(OpCodes.Ldloc, self) //S= count; count; self
                  k.EmitCall(OpCodes.Call, get_p, null) //S= count; count; rule<p>
          
                  let get_Value = p.PropertyType.GetMethod("GetValueMember")
                  do k.EmitCall(OpCodes.Call, get_Value, null) // S = count; count; p
                  k.Emit(OpCodes.Ldsfld, acceleration_index_field) // S = count; count; p; acc
                  k.EmitCall(OpCodes.Call, acceleration_index_field_add, null) //S = count
                | VarType(p_t) | RuleType(p_t) -> 

                  let get_p = p.GetGetMethod()                  
                  k.Emit(OpCodes.Ldloc, self) //S= count; count; self
                  k.EmitCall(OpCodes.Call, get_p, null) //S= count; count; var<p>
                    
                  let get_Value = p.PropertyType.GetMethod("get_Value")
                  do k.EmitCall(OpCodes.Call, get_Value, null) // S = count; count; p
                  k.Emit(OpCodes.Ldsfld, acceleration_index_field) // S = count; count; p; acc
                  k.EmitCall(OpCodes.Call, acceleration_index_field_add, null) //S = count
                | p_t when p_t = typeof<Vector2<_>> -> 

                  let get_p = p.GetGetMethod()
                  k.Emit(OpCodes.Ldloc, self) //S= count; count; self
                  k.EmitCall(OpCodes.Call, get_p, null) //S= count; count; p
                  k.Emit(OpCodes.Ldsfld, acceleration_index_field) // S = count; count; p; acc
                  k.EmitCall(OpCodes.Call, acceleration_index_field_add, null) //S = count

                | _ -> k.Emit(OpCodes.Pop) // S = count

              | None -> failwithf "Position field not found in entity: %s" self_type.Name
              
            do k.Emit(OpCodes.Pop)

          k.Emit(OpCodes.Ldsfld, f) // S = lst
          k.Emit(OpCodes.Ldloc, self) //S = lst ; s
          let add_method = self_type_lst.GetMethod("Add")
          k.EmitCall(OpCodes.Call, add_method, null) //S = empty

    | _ -> ()

    
    let methods = self_type.GetMethods() |> Seq.filter (fun m -> m.Name.EndsWith("Rule") || m.Name.EndsWith("'"))
    let fields = self_type.GetProperties()
    let rec all_fields (t:Type) n = 
      seq{
        for field in t.GetProperties() do
          yield [field]
        if n > 0 then
          for field in t.GetProperties() do
            let fields1 = all_fields field.PropertyType (n-1)
            for field1 in fields1 do
              yield field::field1
      }
    let all_fields = all_fields self_type 3
    let all_fields = all_fields |> Seq.map (fun fields -> fields, fields |> Seq.map (fun f -> f.Name) |> Seq.fold (+) "") |> Seq.toList

    let emit_rule_parameters (m:MethodInfo) (self:LocalBuilder) =
      let is_world t = t = t_world
      let is_self t = t = self_type && t <> t_world
      let is_dt t = t = typeof<float32>
      let rule_parameters =
            [
              is_world, (fun () -> get_world k)                   // S = s;w
              is_self, (fun () -> do k.Emit(OpCodes.Ldloc, self)) // S = s;w;s
              is_dt, (fun () -> do k.Emit(OpCodes.Ldarg_1))       // S = s;w;s;dt
            ]
      for arg in m.GetParameters() do
        match rule_parameters |> Seq.tryFind (fun (p,_) -> p arg.ParameterType) with
        | Some (_,arg_asm) ->
            do arg_asm()
        | _ -> failwithf "Rule %s.%s has an unrecognized argument, %s." self_type.Name m.Name arg.Name

    for m in methods do
      if is_frequency_ok (m.GetCustomAttributes(false)) frequency then
        match all_fields |> Seq.tryFind (fun (fields,names) -> names + "Rule" = m.Name || names + "'" = m.Name) with
        | Some(fields,name) ->
          let method_return_type_ok = 
            match fields.Last.PropertyType with
            | RuleType(arg) ->
              arg = m.ReturnType
            | RuleTableType(arg) ->
              let is = m.ReturnType.GetInterfaces()
              is |> Seq.exists (fun i -> i = typedefof<seq<_>>.MakeGenericType([|arg|])) ||
              m.ReturnType = typedefof<seq<_>>.MakeGenericType([|arg|])
            | VarType(arg) ->
              arg = m.ReturnType
            | arg -> 
              arg = m.ReturnType
          if method_return_type_ok |> not then 
            failwithf "Method rule %s.%s returns a value of %s, which is incompatible with field %s:%s." self_type.Name m.Name m.ReturnType.Name name fields.Last.PropertyType.Name
          let apply,swap = 
            match fields.Last.PropertyType with
            | RuleType(arg) | RuleTableType(arg) | VarType(arg) ->
              fields.Last.PropertyType.GetMethod("Apply"), fields.Last.PropertyType.GetMethod("Swap") 
            | _ when fields.Length = 1 ->
              let field_name = fields.Head.Name
              let apply = self_type.GetMethod("set_" + field_name)
              let swap = null
              in apply,swap
            | _ -> 
              null,null
          do k.Emit(OpCodes.Dup)                    // S = s;s
          match rule_op with
          | Apply ->
            let self = get_local_variable self_type
            do k.Emit(OpCodes.Stloc, self) // S = s
            do emit_rule_parameters m self
            do k.EmitCall(OpCodes.Call, m, null)      // S = s;v'
            let v' = get_local_variable m.ReturnType
            do k.Emit(OpCodes.Stloc, v') // S = s;
            do k.Emit(OpCodes.Dup)                    // S = s;s
            match fields.Last.PropertyType with
            | RuleType(arg) | RuleTableType(arg) | VarType(arg) ->
              for field in fields do
                let get_field = field.GetGetMethod()
                do k.EmitCall(OpCodes.Call, get_field, null) // S = s;f
            | _ -> () // S = s;s
            do k.Emit(OpCodes.Ldloc, v')    // S = s;f|s;v'
            if apply = null && fields.Length = 1 then
              let field = self_type.GetField(fields.Head.Name + "@", BindingFlags.Instance ||| BindingFlags.NonPublic)
              do k.Emit(OpCodes.Stfld, field) // S = s
            else
              do k.EmitCall(OpCodes.Call, apply, null)     // S = s
          | Swap ->
            if swap <> null then
              for field in fields do
                let get_field = field.GetGetMethod()
                do k.EmitCall(OpCodes.Call, get_field, null) // S = s;f
              do k.EmitCall(OpCodes.Call, swap, null)     // S = s            
            else
              do k.Emit(OpCodes.Pop)
        | None -> failwithf "Method rule %s.%s does not have a corresponding field." self_type.Name m.Name


  let type_predicate (t:Type) =
    if types_with_actions.ContainsKey(t) then
      types_with_realtime_rules.[t] || types_with_actions.[t]
    else
      types_with_realtime_rules.[t]
  do traverse_entity false update_RT_il (get_local_variable()) (update_entities RuleOperation.Apply UpdateFrequency.RealTime) type_predicate (fun k -> k.Emit(OpCodes.Ldarg_0)) typeof<'world> typeof<'world>
  do update_RT_il.Emit(OpCodes.Pop)    
  do update_RT_il.Emit(OpCodes.Ret)

  do traverse_entity false swap_RT_il (get_local_variable()) (update_entities RuleOperation.Swap UpdateFrequency.RealTime) (fun t -> types_with_realtime_rules.[t]) (fun k -> k.Emit(OpCodes.Ldarg_0)) typeof<'world> typeof<'world>
  do swap_RT_il.Emit(OpCodes.Pop)
  do swap_RT_il.Emit(OpCodes.Ret)

  for update_il,swap_il,freq,types_pred in [(update_AI_il,swap_AI_il,UpdateFrequency.AI,types_with_ai_rules)
                                            (update_IN_il,swap_IN_il,UpdateFrequency.Interactive,types_with_interactive_rules)] do

    do traverse_entity false update_il (get_local_variable()) (update_entities RuleOperation.Apply freq) (fun t -> types_pred.[t]) (fun k -> k.Emit(OpCodes.Ldarg_0)) typeof<'world> typeof<'world>
    do update_il.Emit(OpCodes.Pop)
    
    do update_il.Emit(OpCodes.Ret)
    do traverse_entity false swap_il (get_local_variable()) (update_entities RuleOperation.Swap freq) (fun t -> types_pred.[t]) (fun k -> k.Emit(OpCodes.Ldarg_0)) typeof<'world> typeof<'world>
    do swap_il.Emit(OpCodes.Pop)
    do swap_il.Emit(OpCodes.Ret)

  //apply actions
  
  let actions() = 
    let get_local_variable () = 
      let num_locals = var 0
      let bound_vars = new System.Collections.Generic.Dictionary<Type * string,int>()
      fun (k:ILGenerator) (t : Type * string) ->
        if bound_vars.ContainsKey t then
          bound_vars.[t]
        else
          do k.DeclareLocal(fst t) |> ignore
          let l = !num_locals
          do num_locals := l + 1
          do bound_vars.Add(t,l)
          l
    let end_for = apply_actions_il.DefineLabel()
    let get_local_variable' = get_local_variable ()
    for action_target in action_targets do
    
      let source_type, source_type_field', _ = action_target.Key
      
      let source_field : FieldBuilder = static_fields_dictionary.[source_type]    
    
      let i = get_local_variable' apply_actions_il (typeof<int>, "action loop")
      do apply_actions_il.Emit(OpCodes.Ldc_I4, 0) // S = 0                                            => int i = 0
      do apply_actions_il.Emit(OpCodes.Stloc, i)  // S = empty                                        => int i = 0

      let get_Count = typedefof<ResizeArray<_>>.MakeGenericType([|source_type|]).GetMethod("get_Count")
      let get_Item = typedefof<ResizeArray<_>>.MakeGenericType([|source_type|]).GetMethod("get_Item")
    
      do apply_actions_il.Emit(OpCodes.Ldsfld, source_field) // S = s_vector    
      let loop = apply_actions_il.DefineLabel()
      let end_loop = apply_actions_il.DefineLabel()
      do apply_actions_il.MarkLabel(loop)
      do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s_vector
      do apply_actions_il.EmitCall(OpCodes.Call, get_Count, null) // S = s_vector ; s_vector_length
      do apply_actions_il.Emit(OpCodes.Ldloc, i) // S = s_vector ; s_vector_length ; i                => i < count
      do apply_actions_il.Emit(OpCodes.Beq, end_loop) // S = s_vector                                 => i < count
      do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s_vector
      do apply_actions_il.Emit(OpCodes.Ldloc, i) // S = s_vector ; s_vector ; i
      do apply_actions_il.EmitCall(OpCodes.Call, get_Item, null) // S = s_vector ; s                  => push s[i]
      do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s ; s                            
      let source_index = get_local_variable' apply_actions_il (source_type, "source")
      do apply_actions_il.Emit(OpCodes.Stloc, source_index) // S = s_vector ; s

      do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s ; s
      let source_position_property = source_type.GetProperties() |> Seq.tryFind(fun p -> p.Name.ToLower() = "position")                        
      match source_position_property with
      | Some position_property ->
        let get_position_property = position_property.GetGetMethod()
        match position_property.PropertyType with
        | RuleType(p_t) ->
          do apply_actions_il.EmitCall(OpCodes.Call, get_position_property, null) // S = s_vector ; s ; rule<p>
          
          let get_Value = position_property.PropertyType.GetMethod("GetValueMember")
          do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p
        | VarType(p_t) ->         
          do apply_actions_il.EmitCall(OpCodes.Call, get_position_property, null) // S = s_vector ; s ; var<p>
          let get_Value = position_property.PropertyType.GetMethod("get_Value")
          do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p
        | p_t when p_t = typeof<Vector2<_>> ->         
          do apply_actions_il.EmitCall(OpCodes.Call, get_position_property, null) // S = s_vector ; s ; p
        | _ -> failwithf "Position field of type %s not supported" source_type.Name

      | None -> failwithf "Position field not found in entity: %s" source_type.Name    
    
      //itero i targets
      let iter_targets() = // S = s_vector ; s ; p
      
        let targets_container_add = targets_container.FieldType.GetMethod("Add")      
      
        //if action does not has a threshold then        
        for target_type in action_target.Value do // S = s_vector ; s ; p  
          if source_type.GetProperty(source_type_field').PropertyType.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<ThresholdAttribute>) |> not then
            let transfers_conditions_list = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<OutputOrTransfer>>()            
            for transfer_property in  source_type.GetProperty(source_type_field').PropertyType.GetCustomAttributes(true) |> Seq.filter (fun a -> a.GetType() = typeof<TransferAttribute>) do
              let transfer_property = transfer_property :?> TransferAttribute
              let transfer_attributes = transfer_property
              if transfers_conditions_list.ContainsKey(transfer_property.From) then
                transfers_conditions_list.[transfer_property.From].Add(Transfer transfer_property)
              else 
                let transfer_list = System.Collections.Generic.List<OutputOrTransfer>()
                do transfer_list.Add(Transfer transfer_property)
                transfers_conditions_list.Add(transfer_property.From, transfer_list)


            let restrictions_list = System.Collections.Generic.List<RestrictAttribute>()            
            for restriction_property in  source_type.GetProperty(source_type_field').PropertyType.GetCustomAttributes(true) |> Seq.filter (fun a -> a.GetType() = typeof<RestrictAttribute>) do
              let restriction_property = restriction_property :?> RestrictAttribute
              let restriction_attributes = restriction_property              
              do restrictions_list.Add(restriction_attributes)
                          
            let target_type_field = static_fields_dictionary.[target_type]
            let get_target_type_item = target_type_field.FieldType.GetMethod("get_Item")

            let clear_method = targets_container.FieldType.GetMethod("Clear")
            
            //clear the target_container        
            do apply_actions_il.Emit(OpCodes.Ldsfld, targets_container)     // S = s_vector ; s ; p ; field
            do apply_actions_il.EmitCall(OpCodes.Call, clear_method, null)  // S = s_vector ; s ; p  

            if source_type.GetProperty(source_type_field').PropertyType.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<RadiusAttribute>) then
              let action = source_type.GetProperty(source_type_field').PropertyType.GetCustomAttributes(true) |> Seq.find (fun a -> a.GetType() = typeof<RadiusAttribute>) :?> RadiusAttribute
              


              let target_type_acceleration_index_field = static_indices_fields_dictionary.[target_type, action.Radius]
              
              
          
              let seq_enumerator_get = typeof<seq<int>>.GetMethod("GetEnumerator")
              let enumerator_move = typeof<System.Collections.IEnumerator>.GetMethod("MoveNext")
              let enumerator_current = typeof<System.Collections.Generic.IEnumerator<int>>.GetMethod("get_Current")

              let acceleration_index_field_get = typeof<AccelerationIndex>.GetMethod("Get")
              do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s ; p ; p
              do apply_actions_il.Emit(OpCodes.Ldsfld, target_type_acceleration_index_field) // S = s_vector ; s ; p ; p ; acc
              do apply_actions_il.EmitCall(OpCodes.Call, acceleration_index_field_get, null) // S = s_vector ; s ; p ; targets
              do apply_actions_il.EmitCall(OpCodes.Callvirt, seq_enumerator_get, null) // S = s_vector ; s ; p ; targets_iterator
          
              let target_iterator_idx = get_local_variable' apply_actions_il (typeof<System.Collections.Generic.IEnumerator<int>>, "iterator loop")
              do apply_actions_il.Emit(OpCodes.Stloc, target_iterator_idx) // S = s_vector ; s ; p
              do apply_actions_il.Emit(OpCodes.Ldloc, target_iterator_idx) // S = s_vector ; s ; p ; targets_iterator          
          
              let target_type_position_property = target_type.GetProperties() |> Seq.find(fun p -> p.Name.ToLower() = "position")
          
              let iterator_loop = apply_actions_il.DefineLabel()
              let end_iterator_loop = apply_actions_il.DefineLabel()

              do apply_actions_il.MarkLabel(iterator_loop) // S = s_vector ; s ; p ; targets_iterator
              do apply_actions_il.Emit(OpCodes.Ldsfld, targets_container) // S = s_vector ; s ; p ; targets_iterator; f
              do apply_actions_il.Emit(OpCodes.Ldloc, target_iterator_idx) // S = s_vector ; s ; p ; targets_iterator ; f ; targets_iterator
              do apply_actions_il.EmitCall(OpCodes.Callvirt, enumerator_move, null) // S = s_vector ; s ; p ; targets_iterator ; f ; bool
              do apply_actions_il.Emit(OpCodes.Brfalse, end_iterator_loop)// S = s_vector ; s ; p ; targets_iterator ; f
              do apply_actions_il.Emit(OpCodes.Ldloc, target_iterator_idx) // S = s_vector ; s ; p ; targets_iterator ; f ; targets_iterator
              do apply_actions_il.EmitCall(OpCodes.Callvirt, enumerator_current, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i

              do apply_actions_il.Emit(OpCodes.Ldsfld, target_type_field) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_list            
              do apply_actions_il.Emit(OpCodes.Ldloc, target_iterator_idx) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_list ; targets_iterator
              do apply_actions_il.EmitCall(OpCodes.Callvirt, enumerator_current, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_list ; i
              do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_list ; i ; i
              let j = get_local_variable' apply_actions_il (typeof<int>, "j - loop")
              do apply_actions_il.Emit(OpCodes.Stloc, j) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_list ; i ;
              do apply_actions_il.EmitCall(OpCodes.Call, get_target_type_item, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t
              do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; t
              let target_index = get_local_variable' apply_actions_il (target_type, "target")
              do apply_actions_il.Emit(OpCodes.Stloc, target_index) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t

              if source_type = target_type then
                let not_same_target_loop = apply_actions_il.DefineLabel()

                do apply_actions_il.Emit(OpCodes.Ldloc, j) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; j
                do apply_actions_il.Emit(OpCodes.Ldloc, i) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; j ; i

                do apply_actions_il.Emit(OpCodes.Sub)  // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; (i - j)
                do apply_actions_il.Emit(OpCodes.Brtrue, not_same_target_loop) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t

                do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ; targets_iterator ; f ; i
                do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ; targets_iterator ; f
                do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ; targets_iterator 

                do apply_actions_il.Emit(OpCodes.Br, iterator_loop) // S = s_vector ; s ; p ; targets_iterator

                do apply_actions_il.MarkLabel(not_same_target_loop) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t

              
                
              if restrictions_list.Count > 0 then                   
                let restriction_label = apply_actions_il.DefineLabel()
                do apply_actions_il.MarkLabel(restriction_label)
                


                do apply_actions_il.Emit(OpCodes.Ldc_I4_1) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true

                for restriction in restrictions_list do                                          

                  do apply_actions_il.Emit(OpCodes.Ldloc, source_index) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; s
                  let source_field_property = source_type.GetProperties() |> Seq.tryFind(fun f -> f.Name.ToLower() = restriction.Field.ToLower())
                  match source_field_property with
                  | Some field_property ->
                    let get_field_property = field_property.GetGetMethod()
                    match field_property.PropertyType with
                    | RuleType(p_t) ->
                      do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; rule<p>          
                      let get_Value = field_property.PropertyType.GetMethod("GetValueMember")
                      do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; p
                    | VarType(t) -> // | RuleType(t) ->         
                      do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; var<f>
                      let get_Value = field_property.PropertyType.GetMethod("get_Value")
                      do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; f
                    | _ -> do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; f
                  | None -> failwithf "%s field not found in entity: %s" restriction.Field source_type.Name 
                    
                  //do apply_actions_il.Emit(OpCodes.Box)
                  do apply_actions_il.Emit(OpCodes.Ldloc, target_index) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; box f ; t

                  let target_field_property = target_type.GetProperties() |> Seq.tryFind(fun f -> f.Name.ToLower() = restriction.Field.ToLower())
                  match target_field_property with
                  | Some field_property ->
                    let get_field_property = field_property.GetGetMethod()
                    match field_property.PropertyType with
                    | RuleType(p_t) ->
                      do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; box f ; rule<p>
                      let get_Value = field_property.PropertyType.GetMethod("GetValueMember")
                      do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; box f ; p
                    | VarType(t) -> // | RuleType(t) ->         
                      do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; box f ; var<f>
                      let get_Value = field_property.PropertyType.GetMethod("get_Value")
                      do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; box f ; f
                    | _ -> do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; box f ; f
                  | None -> failwithf "%s field not found in entity: %s" restriction.Field source_type.Name 
                    
                  //do apply_actions_il.Emit(OpCodes.Box)
                  let eq_property = target_field_property.Value.PropertyType
                  let equals = eq_property.GetMethod("Equals", [|eq_property|]) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; box f ; box f
                  // a b => eq => false

                  do apply_actions_il.EmitCall(OpCodes.Call, equals, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; res
                  if restriction.Condition = Condition.NotEqual then 

                    do apply_actions_il.Emit(OpCodes.Not) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; true ; res

                  do apply_actions_il.Emit(OpCodes.And) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ; res

                let skip_cond = apply_actions_il.DefineLabel()
                do apply_actions_il.Emit(OpCodes.Brtrue, skip_cond) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t ;
                do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; 
                do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ; targets_iterator ; f ; 
                do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ; targets_iterator ;                 
                do apply_actions_il.Emit(OpCodes.Br, iterator_loop)
                
                do apply_actions_il.MarkLabel(skip_cond)
                



              let get_target_position_property = target_type_position_property.GetGetMethod()

              match target_type_position_property.PropertyType with // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t
              | RuleType(p_t) -> // S = s_vector ; s ; p ; targets_iterator ; f ; t 
                  do apply_actions_il.EmitCall(OpCodes.Call, get_target_position_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; t ; rule<p>          
                  let get_Value = target_type_position_property.PropertyType.GetMethod("GetValueMember")
                  do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_p ; s_p

              | VarType(p_t) -> // S = s_vector ; s ; p ; targets_iterator ; f ; t ; t
                do apply_actions_il.EmitCall(OpCodes.Call, get_target_position_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; var<t_p>
                let get_target_position_Value = target_type_position_property.PropertyType.GetMethod("get_Value")
                do apply_actions_il.EmitCall(OpCodes.Call, get_target_position_Value, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_p      
             
              | p_t when p_t = typeof<Vector2<_>> ->  // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t
                do apply_actions_il.EmitCall(OpCodes.Call, get_target_position_property, null) // // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_p
            
          
              //fill the targets container
              do apply_actions_il.Emit(OpCodes.Ldloc, source_index) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_p ; s
              let distance = typeof<Vector2<_>>.GetMethod("Distance")            
              match source_position_property with
              | Some position_property ->
                let get_position_property = position_property.GetGetMethod()
                match position_property.PropertyType with                
                | RuleType(p_t) ->
                  do apply_actions_il.EmitCall(OpCodes.Call, get_position_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_p ; rule<p>          
                  let get_Value = position_property.PropertyType.GetMethod("GetValueMember")
                  do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_p ; s_p
                | VarType(p_t) ->         
                  do apply_actions_il.EmitCall(OpCodes.Call, get_position_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_p ; var<p>
                  let get_Value = position_property.PropertyType.GetMethod("get_Value")
                  do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_p ; s_p                  
                
                | p_t when p_t = typeof<Vector2<_>> ->         
                  do apply_actions_il.EmitCall(OpCodes.Call, get_position_property, null) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; t_p ; s_p
                  
                
                | _ -> failwithf "Position field of type %s not supported" source_type.Name
                do apply_actions_il.EmitCall(OpCodes.Call, distance, null)  // S = s_vector ; s ; p ; targets_iterator ; f ; i ; dis_s_p
              | None -> failwithf "Position field not found in entity: %s" source_type.Name    
            
              do apply_actions_il.Emit(OpCodes.Ldc_R4, action.Radius) // S = s_vector ; s ; p ; targets_iterator ; f ; i ; dis_s_p ; radius
            
              let add_to_container_label = apply_actions_il.DefineLabel()            
              do apply_actions_il.Emit(OpCodes.Ble, add_to_container_label) // S = s_vector ; s ; p ; targets_iterator ; f ; i
              do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ; targets_iterator ; f 
              do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ; targets_iterator ; 
                       
              do apply_actions_il.Emit(OpCodes.Br, iterator_loop) // S = s_vector ; s ; p ; targets_iterator ; 

              do apply_actions_il.MarkLabel(add_to_container_label)
              do apply_actions_il.EmitCall(OpCodes.Call, targets_container_add, null) // S = s_vector ; s ; p ; targets_iterator;

              do apply_actions_il.Emit(OpCodes.Br, iterator_loop) // S = s_vector ; s ; p ; targets_iterator;

              do apply_actions_il.MarkLabel(end_iterator_loop) // S = s_vector ; s ; p ; targets_iterator ; f   
              do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ; targets_iterator
              do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p            

              let get_Count = typeof<System.Collections.Generic.List<int>>.GetMethod("get_Count")
              let get_Item = typeof<System.Collections.Generic.List<int>>.GetMethod("get_Item")

              let i' = get_local_variable' apply_actions_il (typeof<int>, "inner loop - acceleration_indexes")
              do apply_actions_il.Emit(OpCodes.Ldc_I4, 0) // S = s_vector ; s ; p ; 0                                            => int i = 0
              do apply_actions_il.Emit(OpCodes.Stloc, i') // S = s_vector ; s ; p ;                                              => int i = 0            
              do apply_actions_il.Emit(OpCodes.Ldsfld, targets_container) // S = s_vector ; s ; p ; tgt_cont_vector
              let loop = apply_actions_il.DefineLabel()
              let end_loop = apply_actions_il.DefineLabel()
              do apply_actions_il.MarkLabel(loop)
              do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s ; p ; tgt_cont_vector ; tgt_cont_vector
              do apply_actions_il.EmitCall(OpCodes.Call, get_Count, null) // S = s_vector ; s ; p ; tgt_cont_vector ; tgt_cont_vector_length
              do apply_actions_il.Emit(OpCodes.Ldloc, i') // S = s_vector ; s ; p ; tgt_cont_vector ; tgt_cont_vector_length ; i                => i < count
              do apply_actions_il.Emit(OpCodes.Beq, end_loop) // S = s_vector ; s ; p ; tgt_cont_vector                                         => i < count
              do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s ; p ; tgt_cont_vector ; tgt_cont_vector  
              do apply_actions_il.Emit(OpCodes.Ldloc, i') // S = s_vector ; s ; p ; tgt_cont_vector ; tgt_cont_vector ; i
              do apply_actions_il.EmitCall(OpCodes.Call, get_Item, null) // S = s_vector ; s ; p ; tgt_cont_vector ; tgt_cont                   => push s[i]
              let tgt_cont_idx = get_local_variable' apply_actions_il (typeof<int>, "target container index - acceleration_indexes")
              do apply_actions_il.Emit(OpCodes.Stloc, tgt_cont_idx) // S = s_vector ; s ; p ; tgt_cont_vector
              do apply_actions_il.Emit(OpCodes.Ldsfld, target_type_field) // S = s_vector ; s ; p ; tgt_cont_vector ; target_field
              do apply_actions_il.Emit(OpCodes.Ldloc, tgt_cont_idx) // S = s_vector ; s ; p ; tgt_cont_vector ; target_field ; tgt_cont
              do apply_actions_il.EmitCall(OpCodes.Call, get_target_type_item, null) // S = s_vector ; s ; p ; tgt_cont_vector ; t
              //do apply_actions_il.Emit(OpCodes.Pop)

              let t_idx = get_local_variable' apply_actions_il (target_type, "selected target - acceleration_indexes")
              do apply_actions_il.Emit(OpCodes.Stloc, t_idx)                  // S = s_vector ; s ; p ; tgt_cont_vector

              do transfer_action transfers_conditions_list None source_type target_type apply_actions_il source_index t_idx get_local_variable'// S = s_vector ; s ; p ; tgt_cont_vector
              do insert_action source_type source_type_field'  target_type apply_actions_il source_index t_idx get_local_variable'
                       
              do apply_actions_il.Emit(OpCodes.Ldloc, i') // S = s_vector ; s ; p ; tgt_cont_vector ; i
              do apply_actions_il.Emit(OpCodes.Ldc_I4, 1) // S = s_vector ; s ; p ; tgt_cont_vector ; i ; 1
              do apply_actions_il.Emit(OpCodes.Add) // S = s_vector ; s ; p ; tgt_cont_vector ; (i + 1)                                 => i++
              do apply_actions_il.Emit(OpCodes.Stloc, i')  // S = s_vector ; s ; p ; tgt_cont_vector
              do apply_actions_il.Emit(OpCodes.Br, loop)

              do apply_actions_il.MarkLabel(end_loop) // S = s_vector ; s ; p ; tgt_cont_vector
              do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ;

            else // S = s_vector ; s ; p  
              //do not use the spatial optimization

              let get_Count = target_type_field.FieldType.GetMethod("get_Count")

              let i' = get_local_variable' apply_actions_il (typeof<int>, "inner loop - without_acceleration_indexes")
              do apply_actions_il.Emit(OpCodes.Ldc_I4, 0) // S = s_vector ; s ; p ; 0                                            => int i = 0
              do apply_actions_il.Emit(OpCodes.Stloc, i') // S = s_vector ; s ; p ;                                              => int i = 0            
              do apply_actions_il.Emit(OpCodes.Ldsfld, target_type_field) // S = s_vector ; s ; p ; target_field
              let loop = apply_actions_il.DefineLabel()
              let end_loop = apply_actions_il.DefineLabel()
              do apply_actions_il.MarkLabel(loop)
              do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s ; p ; target_field ; target_field
              do apply_actions_il.EmitCall(OpCodes.Call, get_Count, null) // S = s_vector ; s ; p ; target_field ; target_field_length
              do apply_actions_il.Emit(OpCodes.Ldloc, i') // S = s_vector ; s ; p ; target_field ; target_field_length ; i                      => i < count
              do apply_actions_il.Emit(OpCodes.Beq, end_loop) // S = s_vector ; s ; p ; target_field                                            => i < count
              do apply_actions_il.Emit(OpCodes.Dup) // S = s_vector ; s ; p ; target_field ; target_field  
              do apply_actions_il.Emit(OpCodes.Ldloc, i') // S = s_vector ; s ; p ; target_field ; target_field ; i                             => push s[i]
              do apply_actions_il.EmitCall(OpCodes.Call, get_target_type_item, null) // S = s_vector ; s ; p ; target_field ; t

              let t_idx = get_local_variable' apply_actions_il (target_type, "selected target - without_acceleration_indexes")
              do apply_actions_il.Emit(OpCodes.Stloc, t_idx)                  // S = s_vector ; s ; p ; target_field 

              do transfer_action transfers_conditions_list None source_type target_type apply_actions_il source_index t_idx get_local_variable'// S = s_vector ; s ; p ; target_field 
              do insert_action source_type source_type_field'  target_type apply_actions_il source_index t_idx get_local_variable'
                       
              do apply_actions_il.Emit(OpCodes.Ldloc, i') // S = s_vector ; s ; p ; target_field ; i
              do apply_actions_il.Emit(OpCodes.Ldc_I4, 1) // S = s_vector ; s ; p ; target_field ; i ; 1
              do apply_actions_il.Emit(OpCodes.Add) // S = s_vector ; s ; p ; target_field ; (i + 1)                                 => i++
              do apply_actions_il.Emit(OpCodes.Stloc, i')  // S = s_vector ; s ; p ; target_field 
              do apply_actions_il.Emit(OpCodes.Br, loop)

              do apply_actions_il.MarkLabel(end_loop) // S = s_vector ; s ; p ; target_field 
              do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s ; p ;      

          //the action has a threshold restriction, it works only on self
          else // S = s_vector ; s ; p
            let source_type, source_field, action_attribute = action_target.Key          
            let transfers_conditions_list = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<OutputOrTransfer>>()            
            for transfer_property in  source_type.GetProperty(source_field).PropertyType.GetCustomAttributes(true) |> Seq.filter (fun a -> a.GetType() = typeof<TransferAttribute>) do
              let transfer_property = transfer_property :?> TransferAttribute
              let transfer_attributes = transfer_property
              if transfers_conditions_list.ContainsKey(transfer_property.From) then
                transfers_conditions_list.[transfer_property.From].Add(Transfer transfer_property)
              else 
                let transfer_list = System.Collections.Generic.List<OutputOrTransfer>()
                do transfer_list.Add(Transfer transfer_property)
                transfers_conditions_list.Add(transfer_property.From, transfer_list)
            
            let threshold_not_reached = apply_actions_il.DefineLabel()            
            let thresholds_conditions_list = System.Collections.Generic.Dictionary<string, ThresholdAttribute>()

            for threshold_property in  source_type.GetProperty(source_field).PropertyType.GetCustomAttributes(true) |> Seq.filter (fun a -> a.GetType() = typeof<ThresholdAttribute>) do
              let threshold_property = threshold_property :?> ThresholdAttribute
              let transfer_attributes = threshold_property                                                      
              thresholds_conditions_list.Add(threshold_property.Field, threshold_property)

            do transfer_action transfers_conditions_list (Some thresholds_conditions_list) source_type source_type apply_actions_il source_index source_index get_local_variable' // S = s_vector ; s ; p ;
            do insert_action source_type source_type_field' source_type apply_actions_il source_index source_index get_local_variable'

            let thresholds_conditions_list = System.Collections.Generic.List<ThresholdAttribute>()            
            for threshold_property in  source_type.GetProperty(source_field).PropertyType.GetCustomAttributes(true) |> Seq.filter (fun a -> a.GetType() = typeof<ThresholdAttribute>) do
              let threshold_property = threshold_property :?> ThresholdAttribute
              let transfer_attributes = threshold_property                                                      
              thresholds_conditions_list.Add(threshold_property)

            do apply_actions_il.Emit(OpCodes.Ldc_I4_1) // S = s_vector ; s ; p ; true

            for threshold_condition in thresholds_conditions_list do                            
              do apply_actions_il.Emit(OpCodes.Ldloc, source_index) // S = s_vector ; s ; p ; true ; s 
              let source_field_property = source_type.GetProperties() |> Seq.tryFind(fun f -> f.Name.ToLower() = threshold_condition.Field.ToLower())
              match source_field_property with
              | Some field_property ->
                let get_field_property = field_property.GetGetMethod()
                match field_property.PropertyType with
                | RuleType(p_t) ->
                  do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; true ; rule<p>          
                  let get_Value = field_property.PropertyType.GetMethod("GetValueMember")
                  do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; true ; p
                | VarType(t) ->
                  do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; true ; var<f>
                  let get_Value = field_property.PropertyType.GetMethod("get_Value")
                  do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; true ; f
                | _ -> do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; true ; f
              | None -> failwithf "%s field not found in entity: %s" threshold_condition.Field source_type.Name 
              
              if System.Int32.TryParse(threshold_condition.Value) |> fst then 
                do apply_actions_il.Emit(OpCodes.Ldc_I4, System.Int32.TryParse(threshold_condition.Value) |> snd) // S = s_vector ; s ; p ; true ; f ; n
              elif System.Single.TryParse(threshold_condition.Value) |> fst then 
                let res = System.Single.TryParse(threshold_condition.Value)
                do apply_actions_il.Emit(OpCodes.Ldc_R4, System.Single.TryParse(threshold_condition.Value) |> snd) // S = s_vector ; s ; p ; true ; f ; n
              elif System.Boolean.TryParse(threshold_condition.Value) |> fst then 
                let bool_val = if System.Boolean.TryParse(threshold_condition.Value) |> snd then 1 else 0 // S = s_vector ; s ; p ; true ; f ; n
                do apply_actions_il.Emit(OpCodes.Ldc_I4, bool_val)
              else
                let source_field_property = source_type.GetProperties() |> Seq.tryFind(fun f -> f.Name.ToLower() = threshold_condition.Value.ToLower())
                match source_field_property with
                |Some field_property ->        
                  do apply_actions_il.Emit(OpCodes.Ldloc, source_index) // S = s_vector ; s ; p ; true ; f ; s                  
                  let get_field_property = field_property.GetGetMethod()
                  match field_property.PropertyType with
                  | RuleType(p_t) ->

                    do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; true ; f ; rule<p>
                    let get_Value = field_property.PropertyType.GetMethod("GetValueMember")
                    do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; true ; f ; p

                  | VarType(t) -> 

                    do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; true ; f ; var<s_to>
                    let get_Value = field_property.PropertyType.GetMethod("get_Value")
                    do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = s_vector ; s ; p ; true ; f ; s_to
                  | _ -> do apply_actions_il.EmitCall(OpCodes.Call, get_field_property, null) // S = s_vector ; s ; p ; true ; f ; s_to
                |None -> failwithf "Threshold value not supported. Type: %s Value: %s" source_type.Name threshold_condition.Value

              do apply_actions_il.Emit(OpCodes.Cgt) // S = s_vector ; s ; p ; true ; res
              do apply_actions_il.Emit(OpCodes.And) // S = s_vector ; s ; p ; res

            do apply_actions_il.Emit(OpCodes.Brfalse,threshold_not_reached) // S = s_vector ; s ; p ;

            if action_attribute.Target.ToLower() = "self" then


              let transfers_conditions_list = System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<OutputOrTransfer>>()
              for transfer_property in  source_type.GetProperty(source_type_field').PropertyType.GetCustomAttributes(true) |> Seq.filter (fun a -> a.GetType() = typeof<OutputAttribute>) do
                let transfer_property = transfer_property :?> OutputAttribute
                let transfer_attributes = transfer_property
                if transfers_conditions_list.ContainsKey(transfer_property.Value) then
                  transfers_conditions_list.[transfer_property.Value].Add(Output transfer_property)
                else 
                  let transfer_list = System.Collections.Generic.List<OutputOrTransfer>()
                  do transfer_list.Add(Output transfer_property)
                  transfers_conditions_list.Add(transfer_property.Value, transfer_list)

              do transfer_action transfers_conditions_list None source_type source_type apply_actions_il source_index source_index get_local_variable'// S = s_vector ; s ; p ;
            else
              do failwithf "Threshold output to targets different than self not supported. Type: %s" source_type.Name


            do apply_actions_il.MarkLabel(threshold_not_reached)

      do iter_targets() // S = s_vector ; s ; p

      do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector ; s
      do apply_actions_il.Emit(OpCodes.Pop) // S = s_vector
      do apply_actions_il.Emit(OpCodes.Ldloc, i) // S = s_vector ; i
      do apply_actions_il.Emit(OpCodes.Ldc_I4, 1) // S = s_vector ; i ; 1
      do apply_actions_il.Emit(OpCodes.Add) // S = s_vector ; (i + 1)                                 => i++
      do apply_actions_il.Emit(OpCodes.Stloc, i)  // S = s_vector 
      do apply_actions_il.Emit(OpCodes.Br, loop)
      do apply_actions_il.MarkLabel(end_loop)
      do apply_actions_il.Emit(OpCodes.Pop) // S = empty

  do actions()  

  for static_acc_field in static_indices_fields_dictionary do
    let static_field = static_acc_field.Value
    let clear_method = static_field.FieldType.GetMethod("Clear")
    
    do apply_actions_il.Emit(OpCodes.Ldsfld, static_field) // S = field
    do apply_actions_il.EmitCall(OpCodes.Call, clear_method, null) // S = empty
    

  
  for static_field in static_fields_dictionary do
    let static_field = static_field.Value
    let clear_method = static_field.FieldType.GetMethod("Clear")

    do apply_actions_il.Emit(OpCodes.Ldsfld, static_field)
    do apply_actions_il.EmitCall(OpCodes.Call, clear_method, null)
  
  do apply_actions_il.Emit(OpCodes.Ret)

  //fine actions

  do pre_draw.DefineParameter(1, ParameterAttributes.In, "world") |> ignore
  let pre_draw_il = pre_draw.GetILGenerator(256)
  do pre_draw_il.Emit(OpCodes.Ldarg_0)
  let pre_draw_drawables _ (self_type:System.Type) (k:ILGenerator) (get_world:ILGenerator->Unit) t_world =
    let t_a = self_type
    if t_a.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_methods = t_a.GetMethods()
      let t_clear = t_a.GetMethod("Clear")
      let t_register = t_a.GetMethod("Register")     
      do k.Emit(OpCodes.Dup)
      do k.Emit(OpCodes.Dup)
      do k.EmitCall(OpCodes.Call, t_clear, null)
      do k.EmitCall(OpCodes.Call, t_register, null)
  do traverse_entity false pre_draw_il (get_local_variable()) pre_draw_drawables (fun t -> types_with_draw.[t]) (fun k -> k.Emit(OpCodes.Ldarg_0)) typeof<'world> typeof<'world>
  do pre_draw_il.Emit(OpCodes.Pop)
  do pre_draw_il.Emit(OpCodes.Ret)

  do draw.DefineParameter(1, ParameterAttributes.In, "world") |> ignore
  let draw_il = draw.GetILGenerator(256)
  do draw_il.Emit(OpCodes.Ldarg_0)
  

  let draw_drawables _ (self_type:System.Type) (k:ILGenerator) (get_world:ILGenerator->Unit) t_world =
    let t_a = self_type
    if t_a.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_methods = t_a.GetMethods()
      match t_methods |> Seq.tryFind (fun m -> m.Name = "Draw") with
      | Some t_draw -> 
        do k.Emit(OpCodes.Dup)
        do k.EmitCall(OpCodes.Call, t_draw, null)
      | None -> ()
  do traverse_entity false draw_il (get_local_variable()) draw_drawables (fun t -> types_with_draw.[t]) (fun k -> k.Emit(OpCodes.Ldarg_0)) typeof<'world> typeof<'world>
  do draw_il.Emit(OpCodes.Pop)
  do draw_il.Emit(OpCodes.Ret)

  let reload_drawable (get_local_variable:ILGenerator->Type->LocalBuilder) (self_type:System.Type) (k:ILGenerator) (get_world:ILGenerator->Unit) t_world =
    let t_a = self_type
    if self_type.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_methods = t_a.GetMethods()
      match t_methods |> Seq.tryFind (fun m -> m.Name = "Reload") with
      | Some t_reload when t_reload.GetParameters().Length = 0 -> 
        do k.Emit(OpCodes.Dup) // s;s
        do k.EmitCall(OpCodes.Call, t_reload, null)
      | _ -> ()

  let reload_layer (get_local_variable:ILGenerator->Type->LocalBuilder) (self_type:System.Type) (k:ILGenerator) (get_world:ILGenerator->Unit) t_world =
    let t_a = self_type
    if self_type.GetCustomAttributes(true) |> Seq.exists (fun a -> a.GetType() = typeof<CasanovaDrawable>) then
      let t_methods = t_a.GetMethods()
      match t_methods |> Seq.tryFind (fun m -> m.Name = "Reload") with
      | Some t_reload when t_reload.GetParameters().Length > 0 -> 
        do k.Emit(OpCodes.Dup) // s;s
        do k.Emit(OpCodes.Ldarg, 2) // s;s;cm
        do k.Emit(OpCodes.Ldarg, 3) // s;s;cm;gd
        do k.EmitCall(OpCodes.Call, t_reload, null)
      | _ -> ()

  do traverse_entity true reload_drawables_il (get_local_variable()) reload_drawable (fun t -> types_with_draw.[t]) (fun k -> k.Emit(OpCodes.Ldarg_0)) typeof<'world> typeof<'world>
  do reload_drawables_il.Emit(OpCodes.Pop)
  do reload_drawables_il.Emit(OpCodes.Ret)

  do traverse_entity true reload_layers_il (get_local_variable()) reload_layer (fun t -> types_with_draw.[t]) (fun k -> k.Emit(OpCodes.Ldarg_0)) typeof<'world> typeof<'world>
  do reload_layers_il.Emit(OpCodes.Pop)
  do reload_layers_il.Emit(OpCodes.Ret)
    
  let program = programmclass.CreateType()
  assemblybuilder.Save(name)

  let update_script (main:Var<_>) = main.Value <- Coroutines.update_script main.Value

  let update_RT = program.GetMethod("Update_RT" + suffix).CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  let update_AI = program.GetMethod("Update_AI" + suffix).CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  let update_IN = program.GetMethod("Update_IN" + suffix).CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  let swap_RT = program.GetMethod("Swap_RT" + suffix).CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  let swap_AI = program.GetMethod("Swap_AI" + suffix).CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  let swap_IN = program.GetMethod("Swap_IN" + suffix).CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  let pre_draw  = program.GetMethod("PreDraw" + suffix).CreateDelegate(typeof<Action<'world>>) :?> Action<'world>
  let update_RT = program.GetMethod("Update_RT" + suffix).CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  let draw = program.GetMethod("Draw" + suffix).CreateDelegate(typeof<Action<'world>>) :?> Action<'world>
  let reload_drawables  = program.GetMethod("ReloadDrawables" + suffix).CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>  
  let reload_layers = program.GetMethod("ReloadLayers" + suffix).CreateDelegate(typeof<Action<'world,float32,ContentManager,GraphicsDevice>>) :?> Action<'world,float32,ContentManager,GraphicsDevice>
  let apply_actions = program.GetMethod("ApplyActions" + suffix).CreateDelegate(typeof<Action<float32>>) :?> Action<float32>
  
  //let reload_layers = programmclass.DefineMethod("ReloadLayers"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<'world>;typeof<float32>;typeof<ContentManager>;typeof<GraphicsDevice>|])
  //let apply_actions = programmclass.DefineMethod("ApplyActions"+ suffix, MethodAttributes.Public ||| MethodAttributes.Static, null, [|typeof<float32>|])
  //let _update_RT = update_RT.CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  //let update_AI = update_AI.CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  //let update_IN = update_IN.CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  //let swap_RT = swap_RT.CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  //let swap_AI = swap_AI.CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  //let swap_IN = swap_IN.CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  //let pre_draw = pre_draw.CreateDelegate(typeof<Action<'world>>) :?> Action<'world>
  //let draw = draw.CreateDelegate(typeof<Action<'world>>) :?> Action<'world>
  //let reload_drawables = reload_drawables.CreateDelegate(typeof<Action<'world,float32>>) :?> Action<'world,float32>
  //let reload_layers = reload_layers.CreateDelegate(typeof<Action<'world,float32,ContentManager,GraphicsDevice>>) :?> Action<'world,float32,ContentManager,GraphicsDevice>


  let inline timer t max_t dt a not_a = 
    if !t >= max_t then
      do t := !t - max_t
      do a()
    else
      do not_a()
    do t := !t + dt
  let ai_tick_timer = var 0.0f
  let input_tick_timer = var 0.0f
  let update (dt:float32) = 
    do update_RT.Invoke(world, dt)
    do apply_actions.Invoke(dt)
    let dt = dt / time_speed
    do update_script main
    do step_scripts()
    do timer ai_tick_timer ai_tick_interval dt
              (fun () -> update_AI.Invoke(world, (ai_tick_interval * time_speed))
                         step_ai_scripts())
              (fun () -> swap_AI.Invoke(world,(ai_tick_interval * time_speed)))
    do timer input_tick_timer input_tick_interval dt
              (fun () -> update_IN.Invoke(world, (input_tick_interval * time_speed))
                         step_input_scripts())
              (fun () -> swap_IN.Invoke(world,(input_tick_interval * time_speed)))
    
    do commit_rule_updates()
  let draw () =
    default_layer.Clear()
    default_layer.Register()
    do pre_draw.Invoke(world)
    default_layer.Draw()
    do draw.Invoke(world) |> ignore
  let reload world content graphics_device =
    reload_layers.Invoke(world,0.0f,content,graphics_device)
    reload_drawables.Invoke(world,0.0f)
  update,draw,reload
