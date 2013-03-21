module Casanova.Action
open Casanova
open Casanova.TypePredicates
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Reflection
open System



type Operation = Add = 0 | Subtract = 1 | Set = 2
type Condition = Equal = 0 | NotEqual = 1

type ResourcesAttribute() =
  class
  end

type ActionAttribute() =
  class
    member val Target : string = null with get,set
  end

[<System.AttributeUsage(System.AttributeTargets.All, AllowMultiple=true)>]
type RestrictAttribute() =
  class
    member val Condition : Condition = Condition.NotEqual with get,set
    member val Field : string = null with get,set
  end

[<System.AttributeUsage(System.AttributeTargets.All, AllowMultiple=true)>]
type TransferAttribute() =
  class
    member val Operation : Operation = Operation.Add with get,set
    member val From : string = null with get,set
    member val To : string = null with get,set
    member val Multiplier : float32 = 1.0f with get,set
  end

[<System.AttributeUsage(System.AttributeTargets.All, AllowMultiple=true)>]
type OutputAttribute() =
  class
    member val Operation : Operation = Operation.Add with get,set
    member val Value : string = null with get,set
    member val To : string = null with get,set
    member val Multiplier : float32 = 1.0f with get,set
  end

[<System.AttributeUsage(System.AttributeTargets.All, AllowMultiple=true)>]
type InsertAttribute() =
  class    
    member val Value : string = null with get,set
    member val To : string = null with get,set    
  end

[<System.AttributeUsage(System.AttributeTargets.All, AllowMultiple=true)>]
type ThresholdAttribute() =
  class
    member val Field    : string = null with get,set
    member val Value    : string = null with get,set
  end

type RadiusAttribute(radius:float32) =
  class
    member val Radius    : float32 = radius with get,set
  end

type OutputOrTransfer =
  | Transfer of TransferAttribute
  | Output of OutputAttribute


//transfer code
let insert_action   (source_type : Type)
                    (source_type_field : string)
                    (target_type : Type) 
                    (apply_actions_il : ILGenerator)
                    (source_index : int)
                    (target_idx : int)
                    (get_local_variable : ILGenerator -> Type * string -> int) = // S = empty

  do apply_actions_il.Emit(OpCodes.Ldloc, source_index) // S = s
  //let insert_conditions_list : System.Collections.Generic.Dictionary<PropertyInfo, System.Collections.Generic.List<InsertAttribute>> =
  
  let insert_conditions_list =  System.Collections.Generic.Dictionary<PropertyInfo, System.Collections.Generic.List<InsertAttribute>>()
  for insert_property in  source_type.GetProperty(source_type_field).PropertyType.GetCustomAttributes(true) |> Seq.filter (fun a -> a.GetType() = typeof<InsertAttribute>) do
    let insert_property = insert_property :?> InsertAttribute
    let insert_attribute = insert_property
    if insert_conditions_list.ContainsKey(source_type.GetProperty(insert_property.To)) then
      insert_conditions_list.[source_type.GetProperty(insert_property.To)].Add(insert_property)
    else 
      let insert_list = System.Collections.Generic.List<InsertAttribute>()
      do insert_list.Add(insert_attribute)
      insert_conditions_list.Add(source_type.GetProperty(insert_property.To), insert_list)

  for same_to_cond in insert_conditions_list do     // S = s
    let source_to_property = same_to_cond.Key
    for insert_cond in same_to_cond.Value do
      do apply_actions_il.Emit(OpCodes.Dup)         // S = s ; s      

      let target_value_property = target_type.GetProperty(insert_cond.Value)            
      let source_destination_add_method = 
        match source_to_property.PropertyType with
        |RuleTableType(r_t) -> source_to_property.PropertyType.GetMethod("Add")
        |_ -> failwithf "Insert action not supported for not RuleTable type. Type: %s" source_type.Name

      //we know at this point that source_destination_property is of type RuleTable<_>, so i take it's generic argument
      //to compare it with the target_property type
      let source_destination_property_generic_type = source_to_property.PropertyType.GetGenericArguments().[0]      
      match source_destination_property_generic_type, target_value_property.PropertyType with
      | s_t, t_t when s_t = t_t -> () // same type ok!
      | s_t, RuleType(t_t) when s_t = t_t -> () // same type ok!
      | s_t, VarType(t_t) when s_t = t_t -> () // same type ok!
      | s_t, RefType(t_t) when s_t = t_t -> () // same type ok!
      | _ -> failwithf "Insert action error. Value type found: %s expected type: %s" target_value_property.PropertyType.Name source_destination_property_generic_type.Name




      let get_source_to_property = source_to_property.GetGetMethod()
      do apply_actions_il.EmitCall(OpCodes.Call, get_source_to_property, null) // S = s ; table<s>
      let tmp = get_local_variable apply_actions_il (source_to_property.PropertyType, "action source t0")

      do apply_actions_il.Emit(OpCodes.Stloc, tmp)
      do apply_actions_il.Emit(OpCodes.Ldloca, tmp)
      

      do apply_actions_il.Emit(OpCodes.Ldloc, target_idx) // S = s ; table<s> ; t
      let get_target_value_field = target_value_property.GetGetMethod()
      do apply_actions_il.EmitCall(OpCodes.Call, get_target_value_field, null) // S = s ; table<to> ; t_field

      
      match target_value_property.PropertyType with
      |RuleType(r_t) -> 
        let get_target_value_property = target_value_property.PropertyType.GetMethod("GetValueMember")
        do apply_actions_il.EmitCall(OpCodes.Call, get_target_value_property, null) // S = s ; table<to> ; value
        do apply_actions_il.EmitCall(OpCodes.Call, source_destination_add_method, null) // S = s ; 

      |VarType(v_t)  -> 
        let get_target_value_property = target_value_property.PropertyType.GetMethod("get_Value")
        do apply_actions_il.EmitCall(OpCodes.Call, get_target_value_property, null) // S = s ; table<to> ; value
        do apply_actions_il.EmitCall(OpCodes.Call, source_destination_add_method, null) // S = s ; 

      | _ ->                 
        do apply_actions_il.EmitCall(OpCodes.Call, source_destination_add_method, null) // S = s ; 




  do apply_actions_il.Emit(OpCodes.Pop) // S = empty

//transfer code
let transfer_action (transfers_conditions_list : System.Collections.Generic.Dictionary<string, System.Collections.Generic.List<OutputOrTransfer>>) 
                    (maybe_thresholds_list : System.Collections.Generic.Dictionary<string, ThresholdAttribute> option)
                    (source_type : Type)
                    (target_type : Type) 
                    (apply_actions_il : ILGenerator)
                    (source_index : int)
                    (t_idx : int)                    
                    (get_local_variable : ILGenerator -> Type * string -> int) =
  
  for from in transfers_conditions_list do                        // S = empty
    
    for condition in from.Value do                                // S = empty
      let end_cycle = apply_actions_il.DefineLabel()
      let condition_from = 
        match condition with
        | Transfer(t) -> t.From
        | Output(o) -> o.Value
      let condition_to = 
        match condition with
        | Transfer(t) -> t.To
        | Output(o) -> o.To
      let condition_operation =
        match condition with
        | Transfer(t) -> t.Operation
        | Output(o) -> o.Operation
        
      let to_field = target_type.GetProperty(condition_to)        // S = empty

      let get_target_field_Value = 
        match to_field.PropertyType with
        |RuleType(r_t) -> to_field.PropertyType.GetMethod("GetValueMember")
        |_->              to_field.PropertyType.GetMethod("get_Value")

      let set_target_field_Value = 
        match to_field.PropertyType with
        |RuleType(r_t) -> to_field.PropertyType.GetMethod("Apply")
        |_->              to_field.PropertyType.GetMethod("set_Value")



      let to_field = to_field.GetGetMethod()
      do apply_actions_il.Emit(OpCodes.Ldloc, t_idx)              // S = t
      do apply_actions_il.EmitCall(OpCodes.Call, to_field, null)  // S = var<t_to>
      match condition_operation with // S = var<t_to>
      | Operation.Add | Operation.Subtract ->
        do apply_actions_il.Emit(OpCodes.Dup)                     // S = var<t_to> ; var<t_to>
        do apply_actions_il.EmitCall(OpCodes.Call, get_target_field_Value, null) // S = var<t_to> ; t_to
                
        do apply_actions_il.Emit(OpCodes.Dup) // S = var<t_to> ; t_to ; t_to
        
        let skip1 = apply_actions_il.DefineLabel()        
        if get_target_field_Value.ReturnType = typeof<int> then 
          do apply_actions_il.Emit(OpCodes.Ldc_I4, 0) // S = var<t_to> ; t_to ; t_to ; n
          
          do apply_actions_il.Emit(OpCodes.Bge, skip1) // S = var<t_to> ; t_to ;             
          do apply_actions_il.Emit(OpCodes.Pop) // S = var<t_to> ; 
          do apply_actions_il.Emit(OpCodes.Pop) // S = empty
          do apply_actions_il.Emit(OpCodes.Br, end_cycle)

        elif get_target_field_Value.ReturnType = typeof<float32> then 
          do apply_actions_il.Emit(OpCodes.Ldc_R4, 0.0f) // S = var<t_to> ; t_to ; t_to ; f                      
          
          do apply_actions_il.Emit(OpCodes.Bge, skip1) // S = var<t_to> ; t_to ;             
          do apply_actions_il.Emit(OpCodes.Pop) // S = var<t_to> ; 
          do apply_actions_il.Emit(OpCodes.Pop) // S = empty
          do apply_actions_il.Emit(OpCodes.Br, end_cycle)

        else failwithf "Threshold type not supported. Type: %s" get_target_field_Value.ReturnType.Name              
        
        do apply_actions_il.MarkLabel(skip1)   // S = var<t_to> ; t_to ;       

        let skip2 = apply_actions_il.DefineLabel()        

        match maybe_thresholds_list with
        | Some thresholds_list when thresholds_list.ContainsKey(condition_to) ->
          do apply_actions_il.Emit(OpCodes.Dup) // S = var<t_to> ; t_to ; t_to
          let threshold = thresholds_list.[condition_to]          

          if System.Int32.TryParse(threshold.Value) |> fst then 
            do apply_actions_il.Emit(OpCodes.Ldc_I4, System.Int32.TryParse(threshold.Value) |> snd) // S = var<t_to> ; t_to ; t_to ; n
            
            do apply_actions_il.Emit(OpCodes.Ble, skip2) // S = var<t_to> ; t_to ;             
            do apply_actions_il.Emit(OpCodes.Pop) // S = var<t_to> ; 
            do apply_actions_il.Emit(OpCodes.Pop) // S = empty
            do apply_actions_il.Emit(OpCodes.Br, end_cycle)

          elif System.Single.TryParse(threshold.Value) |> fst then 
            do apply_actions_il.Emit(OpCodes.Ldc_R4, System.Single.TryParse(threshold.Value) |> snd) // S = var<t_to> ; t_to ; t_to ; f            

            do apply_actions_il.Emit(OpCodes.Ble, skip2) // S = var<t_to> ; t_to ;             
            do apply_actions_il.Emit(OpCodes.Pop) // S = var<t_to> ; 
            do apply_actions_il.Emit(OpCodes.Pop) // S = empty
            do apply_actions_il.Emit(OpCodes.Br, end_cycle)

          elif System.Boolean.TryParse(threshold.Value) |> fst then
            failwith "Operation Add and Subtract do not support boolean."

        | None -> () 
        
        do apply_actions_il.MarkLabel(skip2)   // S = var<t_to> ; t_to ;

        if System.Int32.TryParse(condition_from) |> fst then 
          do apply_actions_il.Emit(OpCodes.Ldc_I4, System.Int32.TryParse(condition_from) |> snd) // S = var<t_to> ; t_to ; n

        elif System.Single.TryParse(condition_from) |> fst then 
          do apply_actions_il.Emit(OpCodes.Ldc_R4, System.Single.TryParse(condition_from) |> snd) // S = var<t_to> ; t_to ; f

        elif System.Boolean.TryParse(condition_from) |> fst then           
          failwith "Operation Add and Subtract do not support boolean."
        else          
          let from_field = source_type.GetProperty(from.Key)
          
          do apply_actions_il.Emit(OpCodes.Ldloc, source_index) // S = var<t_to> ; t_to ; s
          let from_field_property = from_field.GetGetMethod()
          match from_field.PropertyType with
          | RuleType(p_t) ->

            do apply_actions_il.EmitCall(OpCodes.Call, from_field_property, null) // S = var<t_to> ; t_to ; rule<p>
            let get_Value = from_field.PropertyType.GetMethod("GetValueMember")
            do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = var<t_to> ; t_to ; s_p

          | VarType(t) -> 

            do apply_actions_il.EmitCall(OpCodes.Call, from_field_property, null) // S = var<t_to> ; t_to ; var<s_to>
            let get_Value = from_field.PropertyType.GetMethod("get_Value")
            do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = var<t_to> ; t_to ; s_to
            
          | _ -> do apply_actions_il.EmitCall(OpCodes.Call, from_field_property, null) // S = var<t_to> ; t_to ; s_to
          
        let from_field = 
          if source_type.GetProperty(from.Key) = null then None
          else source_type.GetProperty(from.Key) |> Some
        match condition_operation with // S = var<t_to> ; t_to ; s_to
        | Operation.Add -> 
          do apply_actions_il.Emit(OpCodes.Ldarg, 0) // S =  var<t_to> ; t_to ; s_to ; dt
          do apply_actions_il.Emit(OpCodes.Mul) // S =  var<t_to> ; t_to ; dt_s_to
  
          match from_field with
          | None -> () 
          | Some from_field -> 
            match from_field.PropertyType with 
            | VarType(t) -> 
              let from_field_property = from_field.GetGetMethod()
              do apply_actions_il.Emit(OpCodes.Dup) // S =  var<t_to> ; t_to ; dt_s_to ; dt_s_to
              let resource_exchange = get_local_variable apply_actions_il (typeof<float32>, "Resource Exchange")
              do apply_actions_il.Emit(OpCodes.Stloc, resource_exchange) // S =  var<t_to> ; t_to ; dt_s_to ;
              do apply_actions_il.Emit(OpCodes.Ldloc, source_index) // S =  var<t_to> ; t_to ; dt_s_to ; s
              do apply_actions_il.EmitCall(OpCodes.Call, from_field_property, null) // S =  var<t_to> ; t_to ; dt_s_to ; var<s_to>
              do apply_actions_il.Emit(OpCodes.Dup) // S =  var<t_to> ; t_to ; s_to ; var<s_to> ; var<s_to>
              let get_Value = from_field.PropertyType.GetMethod("get_Value")
              do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S =  var<t_to> ; t_to ; dt_s_to ; var<s_to> ; s_to
              do apply_actions_il.Emit(OpCodes.Ldloc, resource_exchange) // S =  var<t_to> ; t_to ; dt_s_to ; var<s_to> ; s_to ; dt_s_to
              do apply_actions_il.Emit(OpCodes.Sub) // S =  var<t_to> ; t_to ; dt_s_to ; var<s_to> ; (s_to - dt_s_to)
              let set_value = from_field.PropertyType.GetMethod("set_Value")
              do apply_actions_il.EmitCall(OpCodes.Call,set_value, null) // S =  var<t_to> ; t_to ; dt_s_to             
            | _ -> ()

          do apply_actions_il.Emit(OpCodes.Add) // S =  var<t_to> ; (t_to + dt_s_to)
          do apply_actions_il.EmitCall(OpCodes.Call, set_target_field_Value, null) // S =  empty

        | Operation.Subtract -> 
          do apply_actions_il.Emit(OpCodes.Ldarg, 0) // S =  var<t_to> ; t_to ; s_to ; dt
          do apply_actions_il.Emit(OpCodes.Mul) // S =  var<t_to> ; t_to ; dt_s_to

          match from_field with
          | None -> () 
          | Some from_field -> 
            match from_field.PropertyType with
            | VarType(t) -> 
              let from_field_property = from_field.GetGetMethod()
              do apply_actions_il.Emit(OpCodes.Dup) // S =  var<t_to> ; t_to ; dt_s_to ; dt_s_to
              let resource_exchange = get_local_variable apply_actions_il (typeof<float32>, "Resource Exchange")
              do apply_actions_il.Emit(OpCodes.Stloc, resource_exchange) // S =  var<t_to> ; t_to ; dt_s_to ;
              do apply_actions_il.Emit(OpCodes.Ldloc, source_index) // S =  var<t_to> ; t_to ; dt_s_to ; s
              do apply_actions_il.EmitCall(OpCodes.Call, from_field_property, null) // S =  var<t_to> ; t_to ; dt_s_to ; var<s_to>
              do apply_actions_il.Emit(OpCodes.Dup) // S =  var<t_to> ; t_to ; s_to ; var<s_to> ; var<s_to>
              let get_Value = from_field.PropertyType.GetMethod("get_Value")
              do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S =  var<t_to> ; t_to ; dt_s_to ; var<s_to> ; s_to
              do apply_actions_il.Emit(OpCodes.Ldloc, resource_exchange) // S =  var<t_to> ; t_to ; dt_s_to ; var<s_to> ; s_to ; dt_s_to
              do apply_actions_il.Emit(OpCodes.Add) // S =  var<t_to> ; t_to ; dt_s_to ; var<s_to> ; (s_to + dt_s_to)
              let set_value = from_field.PropertyType.GetMethod("set_Value")
              do apply_actions_il.EmitCall(OpCodes.Call,set_value, null) // S =  var<t_to> ; t_to ; dt_s_to             
            | _ -> ()

          do apply_actions_il.Emit(OpCodes.Sub) // S =  var<t_to> ; (t_to - s_to)
          do apply_actions_il.EmitCall(OpCodes.Call, set_target_field_Value, null) // S =  empty 

      | Operation.Set -> // S =  var<t_to>
                  
        if System.Int32.TryParse(condition_from) |> fst then 
          do apply_actions_il.Emit(OpCodes.Ldc_I4, System.Int32.TryParse(condition_from) |> snd) // S =  var<t_to> ; n
        elif System.Single.TryParse(condition_from) |> fst then 
          do apply_actions_il.Emit(OpCodes.Ldc_R4, System.Single.TryParse(condition_from) |> snd) // S =  var<t_to> ; f
        elif System.Boolean.TryParse(condition_from) |> fst then 
          let bool_val = if System.Boolean.TryParse(condition_from) |> snd then 1 else 0 // S =  var<t_to> ; b
          do apply_actions_il.Emit(OpCodes.Ldc_I4, bool_val)
        else
          let from_field = source_type.GetProperty(from.Key)
          do apply_actions_il.Emit(OpCodes.Ldloc, source_index) // S = var<t_to> ; s
          let from_field_property = from_field.GetGetMethod()
          match from_field.PropertyType with
          | RuleType(p_t) ->
            do apply_actions_il.EmitCall(OpCodes.Call, from_field_property, null) // S = var<t_to> ; rule<p>          
            let get_Value = from_field.PropertyType.GetMethod("GetValueMember")
            do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = var<t_to> ; s_p
          | VarType(t) -> //| RuleType(t) ->         
            do apply_actions_il.EmitCall(OpCodes.Call, from_field_property, null) // S = var<t_to> ; var<s_to>
            let get_Value = from_field.PropertyType.GetMethod("get_Value")
            do apply_actions_il.EmitCall(OpCodes.Call, get_Value, null) // S = var<t_to> ; s_to
          
          | _ -> do apply_actions_il.EmitCall(OpCodes.Call, from_field_property, null) // S = var<t_to> ; s


        do apply_actions_il.EmitCall(OpCodes.Call, set_target_field_Value, null) // S = empty
      do apply_actions_il.MarkLabel(end_cycle) // S = empty


let rec fib = function    
    | 0 | 1 -> 1
    | n -> fib (n-1) + fib (n-2)