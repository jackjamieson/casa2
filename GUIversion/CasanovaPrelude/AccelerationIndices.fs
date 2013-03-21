module AccelerationIndices
open System
open Casanova.Math
open Casanova.Utilities


type Pair =
  struct
    val mutable i : int
    val mutable j : int
  end

type AccelerationIndex =
  {
    Width   : float32
    Height  : float32
    Matrix  : ResizeArray<int>[][]
    Dirty   : ResizeArray<Pair>
  } with
  static member Create(width, height, n, m, capacity : int) =
    {
      Width   = width
      Height  = height
      Matrix  = [|for i = 1 to n do
                    yield [| for j = 1 to m do
                              yield ResizeArray<int>(capacity) |]|]
      Dirty   = ResizeArray()
    } 
  member this.Clear() =
    for x in this.Dirty do
      this.Matrix.[x.i].[x.j].Clear()    
    this.Dirty.Clear()

  static member Get(position : Vector2<1>, this) =
    let t = this
    let i = position.X / this.Width |> round |> int
    let j = position.Y / this.Height |> round |> int
    
    seq{ 
      for di = -1 to 1 do
        for dj = -1 to 1 do
          let i' = ((i + di) % this.Matrix.Length + this.Matrix.Length) % this.Matrix.Length
          let j' = ((j + dj) % this.Matrix.[0].Length + this.Matrix.[0].Length) % this.Matrix.[0].Length    
          yield! this.Matrix.[i'].[j']
    } 
    
  
      

  static member Add(elem : int, position : Vector2<1>, this) =
    let i = position.X / this.Width |> round |> int
    let j = position.Y / this.Height |> round |> int
    
    let i = (i % this.Matrix.Length + this.Matrix.Length) % this.Matrix.Length
    let j = (j % this.Matrix.[0].Length + this.Matrix.[0].Length) % this.Matrix.[0].Length

    if this.Matrix.[i].[j].Count = 0 then
      this.Dirty.Add(Pair(i = i, j = j))
    this.Matrix.[i].[j].Add(elem)
    