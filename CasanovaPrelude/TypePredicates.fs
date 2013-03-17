module Casanova.TypePredicates

open System
open System.Reflection
open Casanova.Core

open Microsoft.FSharp.Reflection

/// Partial pattern for reference types that matches on Ref<'a>.
let internal (|Tuple|_|) (t : Type) = 
  if t.IsGenericType &&
     t.FullName.StartsWith("System.Tuple`") then 
     Some(t.GetGenericArguments() |> Array.toList)
  else
    None

/// Partial pattern for reference types that matches on Ref<'a>.
let internal (|RefType|_|) (t : Type) = 
  if t.IsGenericType &&
     t.GetGenericTypeDefinition() = typeof<Ref<_>>.GetGenericTypeDefinition() then 
     Some(t.GetGenericArguments().[0])
  else
    None

/// Partial pattern for union-case types.
let internal (|UnionType|_|) (t : Type) = 
  if t |> FSharpType.IsUnion then 
     Some(FSharpType.GetUnionCases(t))
  else
    None

/// Partial pattern for list types that matches on List<'a>.
let internal (|ListType|_|) (t : Type) = 
  if t.IsGenericType then
     match t.GetInterfaces() |> Seq.tryFind(fun i -> i.IsGenericType && i.GetGenericTypeDefinition() = typedefof<seq<System.Collections.Generic.KeyValuePair<_,_>>>) with
     | Some i -> Some (i.GetGenericArguments().[0])
     | None -> None
  else None

///// Partial pattern for C# list types that matches on ResizeArray<'a>.
//let internal (|ResizeArrayType|_|) (t : Type) = 
//  if t.IsGenericType then
//     if t.GetGenericTypeDefinition() = typedefof<ResizeArray<_>> then
//      Some (t.GetGenericArguments().[0])
//     else 
//      None
//  else None

/// Partial pattern for C# list types that matches on ResizeArray<'a>.
let internal (|ArrayType|_|) (t : Type) = 
  if t.IsArray then
    Some (t.GetElementType())
  else None

/// Partial pattern for option types that matches on Option<'a>.
let internal (|OptionType|_|) (t : Type) = 
  if t.IsGenericType &&
     t.GetGenericTypeDefinition() = typeof<Option<_>>.GetGenericTypeDefinition() then 
     Some (t.GetGenericArguments().[0])
  else None

/// Partial pattern for variable types that matches on Var<'a>.
let internal (|VarType|_|) (t : Type) = 
  if t.IsGenericType &&
     t.GetGenericTypeDefinition() = typeof<Var<_>>.GetGenericTypeDefinition() then 
     Some(t.GetGenericArguments().[0])
  else
    None

/// Partial pattern for rule-table types that matches on RuleTable<'a>.
let internal (|RuleTableType|_|) (t : Type) = 
  if t.IsGenericType &&
     t.GetGenericTypeDefinition() = typeof<RuleTable<_>>.GetGenericTypeDefinition() then 
     Some(t.GetGenericArguments().[0])
  else None

/// Partial pattern for rule types that matches on Rule<'a>.
let internal (|RuleType|_|) (t : Type) = 
  if t.IsGenericType &&
     t.GetGenericTypeDefinition() = typeof<Rule<_>>.GetGenericTypeDefinition() then 
     Some(t.GetGenericArguments().[0])
  else None


