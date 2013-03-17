module Casanova.StandardLibrary.Core

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Casanova
open Casanova.Core
open Casanova.Math
open Casanova.Utilities
open Casanova.Drawing

/// The 2D camera contains a position, a height, a velocity, a climbing velocity,
/// and bounds beyond which it does not go. Set the velocity to start/stop movement,
/// and change the bounds to restrict the camera more or less.
type [<CasanovaEntity>] Camera2D = { 
  Position          : Rule<Vector2<m>>
  Velocity          : Rule<Vector2<m/s>>
  Height            : Rule<float32<m>>
  Climb             : Rule<float32<m/s>>
  Bounds            : Var<Camera2DBounds>
  NetworkingContext : NetworkingContext
} with
  static member Create(bounds)  =
    { 
      Position          = Vector2<m>.Zero |> Rule.Create
      Velocity          = Vector2<m/s>.Zero |> Rule.Create
      Height            = bounds.MinHeight |> Rule.Create
      Climb             = 0.0f<m/s> |> Rule.Create
      Bounds            = var bounds
      NetworkingContext = NetworkingContext()
    }
  static member PositionRule(self:Camera2D,dt:float32<s>) =
    let vel_alpha = !self.Height - (!self.Bounds).MinHeight
    let vel_alpha = 1.0f + vel_alpha * 1.0f<1/m>
    let p' = !self.Position + dt * !self.Velocity * vel_alpha
    let px,py = p'.X,p'.Y
    let px,py = px |> max (self.Bounds.Value.MinPosition.X) |> min (self.Bounds.Value.MaxPosition.X),
                py |> max (self.Bounds.Value.MinPosition.Y) |> min (self.Bounds.Value.MaxPosition.Y)
    in Vector2.Create(px,py)
  static member VelocityRule(self:Camera2D,dt:float32<s>) =
    !self.Velocity * ((1.0f - 20.0f * (dt |> float32)) |> max 0.1f |> min 0.99f)
  static member HeightRule(self:Camera2D,dt:float32<s>) =
    (!self.Height + dt * !self.Climb) |> max (self.Bounds.Value.MinHeight) |> min (self.Bounds.Value.MaxHeight)
  static member ClimbRule(self:Camera2D,dt:float32<s>) =
    !self.Climb * ((1.0f - 20.0f * (dt |> float32)) |> max 0.1f |> min 0.99f)
  member camera.Transform = 
    let position = !camera.Position
    let scale = 1.0f<m> / !camera.Height
    Matrix.CreateTranslation(position.X |> float32, position.Y |> float32, 0.0f) *
      Matrix.CreateScale(scale, scale, 1.0f)


/// The 2D camera bounds are the bounds that define the volume beyond which a 
/// 2D camera does not move.
and [<CasanovaEntity>] Camera2DBounds = { 
    MinPosition       : Vector2<m>
    MaxPosition       : Vector2<m>
    MinHeight         : float32<m>
    MaxHeight         : float32<m>
    NetworkingContext : NetworkingContext
  } with 
    static member Create(min_x:float32<m>, max_x:float32<m>, min_y:float32<m>, max_y:float32<m>, min_h:float32<m>, max_h:float32<m>) =
      { 
        MinPosition       = Vector2<m>.Create(min_x,min_y)
        MaxPosition       = Vector2<m>.Create(max_x,max_y)
        MinHeight         = min_h
        MaxHeight         = max_h
        NetworkingContext = NetworkingContext()
      }      


/// Accumulate the total time since the start of the game
type [<CasanovaEntity>] Timer = {
  TotalTime : Rule<float32<s>>
} with
  static member inline TotalTimeRule(self  : Timer, dt    : float32<s>) =
    !self.TotalTime + dt


/// Compute and draw the smoothed framerate of the game
type [<CasanovaEntity>] FPSCounter = {
  FPSSmoothed         : Rule<float32>
  FPSText             : DrawableText
} with
    static member FPSSmoothedRule(self:FPSCounter,dt:float32<s>) = 
      let dt = dt / time_speed
      MathHelper.Lerp(!self.FPSSmoothed, 1.0f<s> / (dt |> max 0.001f<s>), dt |> float32)
    static member FPSTextStringRule(self:FPSCounter,dt:float32<s>) = 
      (!self.FPSSmoothed).ToString("00#")
    /// Create an fps counter that will be rendered on a specified layer, 
    /// at a given position, with a certain size and a certain color.
    static member Create(layer, font, position, size, color) =
      {
        FPSSmoothed         = Rule.Create(60.0f)
        FPSText             = DrawableText.Create(layer, font, position, Vector2<pixel>.Zero, "?", color, size)
      }
    /// Create an fps counter with an assumed size of 5% of the screen.
    static member Create(layer, font, position, color) =
      FPSCounter.Create(layer, font, position, Vector2<pixel>.One * 0.05f, color)
    /// Create an fps counter with an assumed size of 5% of the screen and black font.
    static member Create(layer, font, position) =
      FPSCounter.Create(layer, font, position, Color.Black)
    /// Create an fps counter with an assumed font of arial and black font.
    static member Create(layer, position, size) =
      FPSCounter.Create(layer, "arial", position, size, Color.Black)


/// A clock that updates its time with rules
type [<CasanovaEntity>] Clock = {
  Time    : Rule<System.DateTime>
} with 
  static member TimeRule(self:Clock,dt:float32<s>) = 
    !self.Time + System.TimeSpan.FromSeconds(dt |> float)
