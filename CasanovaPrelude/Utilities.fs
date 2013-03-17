module Casanova.Utilities

open System

type Random
  with
    /// Generates a random floating-point 32-bit number between 0 and 1.
    member this.NextFloat with get() = this.NextDouble() |> float32

type Microsoft.Xna.Framework.Graphics.Texture2D
  with
    // Gets an XNA vector (without units of measure) with the size of the texture.
    member this.Size with get() = Microsoft.Xna.Framework.Vector2(this.Width |> float32, this.Height |> float32)

type DateTime = System.DateTime
type TimeSpan = System.TimeSpan

let private random = new Random()

/// Generates a random floating-point 64-bit number between 0 and 1.
let random_double() = random.NextDouble()

/// Generates a random floating-point 32-bit number between 0 and 1.
let random_float() = random.NextFloat

/// Generates a random integer.
let random_int() = random.Next()

/// Performs a linear interpolation between two 32-bit floating-point values with unit of measure.
let lerp (a:float32) (x:float32<'u>) (y:float32<'u>) = x * a + y * (1.0f - a)

/// Performs a linear interpolation between two 64-bit floating-point values with unit of measure.
let lerp64 (a:float) (x:float<'u>) (y:float<'u>) = x * a + y * (1.0 - a)

/// Generates a random floating-point 32-bit number between two values with unit of measure.
let random_range : float32<'u> -> float32<'u> -> float32<'u> = fun x y -> lerp (random.NextFloat) x y

/// Generates a random integer between two values.
let random_interval min max = random.Next(min, max)

[<Measure>]
type m

[<Measure>]
type s

[<Measure>]
type km

[<Measure>]
type h

[<Measure>]
type kg

[<Measure>]
type rad

[<Measure>]
type eur

[<Measure>]
type usd

[<Measure>]
type gbp

/// Convert kilometers per hour to meters per second.
let to_meters_per_second (v:float32<km/h>) =
  v * 1000.0f<m/km> / 3600.0f<s/h>

type MathHelper = Microsoft.Xna.Framework.MathHelper

/// Adds units of measure to a floating point number.
let with_measure = LanguagePrimitives.Float32WithMeasure

/// Computes the cosine of a floating point number with unit of measure.
let cos (x:float32<rad>) = cos(x |> float32) |> with_measure

/// Computes the sine of a floating point number with unit of measure.
let sin (x:float32<rad>) = sin(x |> float32) |> with_measure

/// Computes the sigmoid (logistic curve) of a 64-bit floating point number.
let sigmoid x = 1.0 / (1.0 + exp(-x))

/// Computes the smooth interpolation between two numbers
let smoothstep (a:float32) (x:float32<'u>) (y:float32<'u>) : float32<'u> = Microsoft.Xna.Framework.MathHelper.SmoothStep(x |> float32, y |> float32, a) |> with_measure


/// Find the minimum element in a sequence; if the sequence is empty, then return None
let tryMinBy f (s:seq<'a>) =
  if s |> Seq.isEmpty then None
  else Some(s |> Seq.minBy f)

type List<'a> 
  with 
    /// Get a random element; if the list is empty, return None.
    static member TryRandomElement (l:List<'a>) = if l.IsEmpty then None else l.[random_interval 0 l.Length] |> Some

    /// Get a random element of the list.
    static member RandomElement (l:List<'a>) = l.[random_interval 0 l.Length]

    /// Get a random element 'a; the probability of getting an element is the floating point value of the pair.
    static member WeightedRandomElement (l:List<'a * float32>) = 
      let s = ref 0.0f
      let l = 
        [ 
          for (x,px) in l do
            yield x,(!s,!s+px)
            s := !s + px 
        ]      
      let a = random.NextFloat
      match l |> Seq.tryFind (fun (x,(l,u)) -> l <= a && a <= u) with
      | Some (x,_) -> x
      | None -> l |> List.rev |> List.head |> fst

    /// Get the last element of a list.
    member this.Last = this.[this.Length-1]

    /// Create an iterator where each element of the list is paired with its index.
    static member for_i (l:seq<'a>) = l |> Seq.mapi (fun i x -> x,i)

    /// Randomly change the order of the elements of a list
    static member Randomize (l:List<'a>) =
      let l = l |> List.toArray
      for i = 0 to l.Length - 1 do
        let j = random_interval 0 (l.Length - 1)
        let x,y = l.[i],l.[j]
        l.[i] <- y
        l.[j] <- x
      l |> Array.toList

    /// Normalize a list of arbitrary elements; needs a "decompose" 
    /// function to extract the (positive) value to normalize by, and a
    /// "rebuild" function to return the normalized value
    static member inline NormalizeBy decompose (rebuild:_ -> _ -> ^a) (l:List< ^a >) = 
      let sum = l |> Seq.map (decompose >> fst) |> Seq.sum
      [for x in l do  
         let x,y = decompose x
         let x = x / sum
         yield rebuild x y]

    /// Normalize a list of positive values
    static member inline Normalize (l:List<_>) = 
      let sum = l |> Seq.sum
      [ for x in l do yield x / sum ]
      

/// Compute the square of a number.
let inline sqr x = x * x

/// Compute the probability of a value in a Gaussian of a certain average and variance.
let gauss (avg:float32) (lambda:float32) (x:float32) = exp (-sqr(x-avg) * lambda)

/// Compute the entropy in a list of numbers; assumes the list to add to 1.0.
let entropy l = l |> List.sumBy (fun p -> if p > 0.01 then -p * log p else 0.0)

/// Launch the debugger, but only if it is not running already.
let launch_debugger() =
  if System.Diagnostics.Debugger.IsAttached |> not then
    do System.Diagnostics.Debugger.Launch() |> ignore

/// Performs an operation on a value, and then returns the original value.
/// Useful to perform operations such as printing the return value and then returning it: 
/// x |> encapsulate_return print
let encapsulate_return ops ret = 
  do ops ret
  ret


/// Sets a speedup factor for the game dt
let mutable time_speed = 1.0f

