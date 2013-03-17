namespace Casanova.Math

open Microsoft.Xna.Framework

type Matrix = Microsoft.Xna.Framework.Matrix

/// 3D-vector struct with unit of measure.
type Vector3<[<Measure>] 'M> =
    struct
        val v : Vector3

        /// Initialize a 3d-vector where all the components have the same value.
        new(value : float32<'M>) =
            { v = Vector3(float32 value, float32 value, float32 value) }
        /// Initialize a 3d-vector where all the components have a specified value.
        new(x : float32<'M>, y : float32<'M>, z : float32<'M>) =
            { v = Vector3(float32 x, float32 y, float32 z) }
        /// Initialize a 3d-vector where the internal XNA vector has a specified value.
        new(V) = { v = V }

        /// Initialize a 3d-vector where all the components have a certain value (of any unit).
        static member Create(x:float32<'a>, y:float32<'a>,z:float32<'a>) = 
          let x' : float32<'M> = x |> float32 |> LanguagePrimitives.Float32WithMeasure
          let y' : float32<'M> = y |> float32 |> LanguagePrimitives.Float32WithMeasure
          let z' : float32<'M> = y |> float32 |> LanguagePrimitives.Float32WithMeasure
          Vector3<'M>(x',y',z')
        /// Initialize a 3d-vector where all the components have the same value (of any unit).
        static member Create(v:float32<'a>) = Vector3<'M>.Create(v,v,v)
        /// Initialize a 3d-vector from another one (of any unit).
        static member Create(v:Vector3<'a>) = Vector3<'M>.Create(v.X,v.Y,v.Z)
        /// Initialize a 3d-vector with all zeroes.
        static member Create() = Vector3<'M>.Create(0.0f)
        /// Initialize a 3d-vector with all zeroes.
        static member Zero = Vector3<'M>.Create()
        /// Initialize a 3d-vector where all the components are set to one.
        static member One with get() = Vector3<'M>(Vector3.One)
        /// Initialize a 3d-vector to (1,0,0).
        static member UnitX with get() = Vector3<'M>(Vector3.UnitX)
        /// Initialize a 3d-vector to (0,1,0).
        static member UnitY with get() = Vector3<'M>(Vector3.UnitY)
        /// Initialize a 3d-vector to (0,0,1).
        static member UnitZ with get() = Vector3<'M>(Vector3.UnitZ)
        /// Initialize a 3d-vector to (-1,0,0).
        static member Left with get() = - Vector3.UnitX
        /// Initialize a 3d-vector to (1,0,0).
        static member Right with get() = Vector3.UnitX
        /// Initialize a 3d-vector to (0,1,0).
        static member Up with get() = Vector3.UnitY
        /// Initialize a 3d-vector to (0,-1,0).
        static member Down with get() = - Vector3.UnitY
        /// Initialize a 3d-vector to (0,0,1).
        static member Forward with get() = Vector3.UnitZ
        /// Initialize a 3d-vector to (0,0,-1).
        static member Backward with get() = - Vector3.UnitZ
        



        /// Get the first component of the vector.
        member this.X : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.X
        /// Get the second component of the vector.
        member this.Y : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.Y
        /// Get the third component of the vector.
        member this.Z : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.Z
        /// Get the internal XNA-style vector without units of measure.
        member this.ToXNA = this.v

        // Print the vector to a string
        override this.ToString() = this.v.ToString()
    end

type Vector2<[<Measure>] 'M> =
    struct
        val v : Vector2
        /// Initialize a 2d-vector where all the components have the same value (of any unit).
        new(v : float32<'M>) =
            { v = Vector2(float32 v, float32 v) }
        /// Initialize a 2d-vector where all the components have a specified value (of any unit).
        new(x : float32<'M>, y : float32<'M>) =
            { v = Vector2(float32 x, float32 y) }
        /// Initialize a 2d-vector where the internal XNA vector has a specified value.
        new(V) = { v = V }

        /// Initialize a 2d-vector where all the components have a specified value (of any unit).
        static member Create(x:float32<'a>, y:float32<'a>) = 
          let x' : float32<'M> = x |> float32 |> LanguagePrimitives.Float32WithMeasure
          let y' : float32<'M> = y |> float32 |> LanguagePrimitives.Float32WithMeasure
          Vector2<'M>(x',y')
        /// Initialize a 2d-vector where all the components have the same value (of any unit).
        static member Create(v:float32<'a>) = Vector2<'M>.Create(v,v)
        /// Initialize a 2d-vector from another one (of any unit).
        static member Create(v:Vector2<'a>) = Vector2<'M>.Create(v.X,v.Y)
        /// Initialize a 2d-vector where all the components are set to zero.
        static member Create() = Vector2<'M>.Create(0.0f)
        /// Initialize a 2d-vector where all the components have a specified integer value.
        static member Create(x:int,y:int) = Vector2<'M>.Create(x |> float32, y |> float32)

        /// Initialize a 2d-vector where all the components are set to zero.
        static member Zero = Vector2<'M>.Create()
        /// Initialize a 2d-vector where all the components are set to one.
        static member One with get() = Vector2<'M>(Vector2.One)
        /// Initialize a 2d-vector to (1,0).
        static member UnitX with get() = Vector2<'M>(Vector2.UnitX)
        /// Initialize a 2d-vector to (0,1).
        static member UnitY with get() = Vector2<'M>(Vector2.UnitY)

        /// Get the first component of the vector.
        member this.X : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.X
        /// Get the second component of the vector.
        member this.Y : float32<'M> = LanguagePrimitives.Float32WithMeasure this.v.Y
        /// Get the internal XNA-style vector without units of measure.
        member this.ToXNA = this.v

        // Print the vector to a string
        override this.ToString() = this.v.ToString()
    end

[<RequireQualifiedAccessAttribute>]
module internal Vector =
    let add3 (U : Vector3<'M>, V : Vector3<'M>) =
        new Vector3<'M>(U.v + V.v)

    let sub3 (U : Vector3<'M>, V : Vector3<'M>) =
        new Vector3<'M>(U.v - V.v)

    let mul3 (U : Vector3<'M>, V : Vector3<'N>) =
        new Vector3<'M * 'N>(U.X * V.X, U.Y * V.Y, U.Z * V.Z)

    let div3 (U : Vector3<'M>, V : Vector3<'N>) =
        new Vector3<'M / 'N>(U.X / V.X, U.Y / V.Y, U.Z / V.Z)

    let dot3 (U : Vector3<'M>, V : Vector3<'N>) =
        Vector3.Dot(U.v, V.v)
        |> LanguagePrimitives.Float32WithMeasure<'M 'N>

    let cross3 (U : Vector3<'M>, V : Vector3<'N>) =
        let conv = LanguagePrimitives.Float32WithMeasure<'M 'N>
        let temp = Vector3.Cross(U.v, V.v)
        new Vector3<_>(conv temp.X, conv temp.Y, conv temp.Z)

    let len3 (U : Vector3<'M>) =
        LanguagePrimitives.Float32WithMeasure<'M> (U.v.Length())

    let scale3 (k : float32<'K>, U : Vector3<'M>) : Vector3<'K 'M> =
        let conv = LanguagePrimitives.Float32WithMeasure<'K 'M>
        let v = Vector3.Multiply(U.v, float32 k)
        new Vector3<_>(conv v.X, conv v.Y, conv v.Z)

    let normalize3 (U : Vector3<'M>) =
        let len = len3 U
        scale3 ((1.0f / len), U)

    let tryNormalize3 (U : Vector3<'M>) =
        let len = len3 U
        if len > LanguagePrimitives.Float32WithMeasure<'M>(1e-3f) then
            Some <| scale3 ((1.0f/ len), U)
        else
            None

    let add2 (U : Vector2<'M>, V : Vector2<'M>) =
        new Vector2<'M>(U.v + V.v)

    let sub2 (U : Vector2<'M>, V : Vector2<'M>) =
        new Vector2<'M>(U.v - V.v)

    let mul2 (U : Vector2<'M>, V : Vector2<'N>) =
        new Vector2<'M * 'N>(U.X * V.X, U.Y * V.Y)

    let div2 (U : Vector2<'M>, V : Vector2<'N>) =
        new Vector2<'M / 'N>(U.X / V.X, U.Y / V.Y)

    let dot2 (U : Vector2<'M>, V : Vector2<'N>) =
        Vector2.Dot(U.v, V.v)
        |> LanguagePrimitives.Float32WithMeasure<'M 'N>

    let len2 (U : Vector2<'M>) =
        LanguagePrimitives.Float32WithMeasure<'M> (U.v.Length())

    let scale2 (k : float32<'K>, U : Vector2<'M>) : Vector2<'K 'M> =
        let conv = LanguagePrimitives.Float32WithMeasure<'K 'M>
        let v = Vector2.Multiply(U.v, float32 k)
        new Vector2<_>(conv v.X, conv v.Y)

    let distance2 (U : Vector2<'M>, V : Vector2<'M>) =
        let dx = U.v.X - V.v.X
        let dy = U.v.Y - V.v.Y
        sqrt (dx * dx + dy * dy) |> LanguagePrimitives.Float32WithMeasure<'M>

    let normalize2 (U : Vector2<'M>) =
        let len = len2 U
        scale2 ((1.0f / len), U)

    let tryNormalize2 (U : Vector2<'M>) =
        let len = len2 U
        if len > LanguagePrimitives.Float32WithMeasure<'M>(1e-2f) then
            Some <| scale2 ((1.0f/ len), U)
        else
            None

    let lerp (U : Vector2<'M>, V : Vector2<'M>, a : float32) =
        let v = Microsoft.Xna.Framework.Vector2.Lerp(U.v, V.v, a)
        Vector2<'M>.Create(v.X, v.Y)


type Vector3<[<Measure>] 'M>
with
    /// Scale a vector by a scalar.
    static member public (*) (U, k) = Vector.scale3 (k, U)
    /// Scale a vector by the inverse of a scalar.
    static member public (/) (U, k:float32<'v>) = Vector.scale3 (1.0f<1> / k, U)
    /// Multiply two vectors component by component.
    static member public (*) (U, V) = Vector.mul3 (U, V)
    /// Divide two vectors component by component.
    static member public (/) (U, V) = Vector.div3 (U, V)
    /// Add two vectors component by component.
    static member public (+) (U, V) = Vector.add3 (U, V)
    /// Subtract two vectors component by component.
    static member public (-) (U, V) = Vector.sub3 (U, V)
    /// Negate all the components of a vector.
    static member public (~-) (U) = Vector.sub3 (Vector3<'M>.Zero,U)
    /// Get the length of a vector.
    member public this.Length       = this |> Vector.len3
    /// Get the distance between two vectors.
    static member Distance(u,v)     = Vector.sub3(u,v) |> Vector.len3


type Vector2<[<Measure>] 'M>
with
    /// Scale a vector by a scalar.
    static member public (*) (k, U) = Vector.scale2 (k, U)
    /// Scale a vector by a scalar.
    static member public (*) (U, k) = Vector.scale2 (k, U)
    /// Scale a vector by the inverse of a scalar.
    static member public (/) (U, k:float32<'v>) = Vector.scale2 (1.0f<1> / k, U)
    /// Scale a vector by the inverse of a scalar.
    static member public (/) (k:float32<'v>, U) = Vector.scale2 (1.0f<1> / k, U)
    /// Multiply two vectors component by component.
    static member public (*) (U, V) = Vector.mul2 (U, V)
    /// Divide two vectors component by component.
    static member public (/) (U, V) = Vector.div2 (U, V)
    /// Add two vectors component by component.
    static member public (+) (U, V) = Vector.add2 (U, V)
    /// Subtract two vectors component by component.
    static member public (-) (U, V) = Vector.sub2 (U, V)
    /// Negate all the components of a vector.
    static member public (~-) (U) = Vector.sub2 (Vector2<'M>.Zero,U)
    /// Get the length of a vector.
    member public this.Length       = this |> Vector.len2
    /// Get a normalized vector.
    member public this.Normalized   = this / this.Length
    /// Get a normalized vector.
    static member public Normalize(this:Vector2<'M>)   = this / this.Length
    /// Get the distance between two vectors.
    static member Distance(u:Vector2<'M>,v)     = Vector.sub2(u,v) |> Vector.len2
    /// Linearly interpolate two vectors
    static member Lerp(u:Vector2<'M>,v:Vector2<'M>,a)     = Vector.lerp(u,v,a)
    /// Get the dot product between two vectors
    static member Dot(u:Vector2<'M>,v:Vector2<'M>)     = Vector.dot2(u,v)
