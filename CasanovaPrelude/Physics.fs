module Casanova.StandardLibrary.Physics

open Casanova
open Casanova.Utilities
open Casanova.Core
open Casanova.Coroutines
open Casanova.Input
open Casanova.Math


/// A physical entity contains a position, a target to reach, a current velocity,
/// and its maximum movement speed. It accelerates to its maximum speed and then
/// stops when it reaches its target.
type [<CasanovaEntity>] PhysicalEntity = { 
  Position  : Rule<Vector2<m>>
  Target    : Var<Vector2<m>>
  Velocity  : Rule<Vector2<m/s>>
  MaxSpeed  : Var<float32<m/s>>
} with
  member inline this.SetTarget(target : Vector2<m>) =
    this.Velocity := Vector2.Create(0.0f)
    this.Target := target
  static member PositionRule(self:PhysicalEntity,dt:float32<s>) =
    let target_distance = Vector2<m>.Distance(!self.Position,!self.Target)
    let new_position = !self.Position + dt * !self.Velocity
    let new_target_distance = Vector2<m>.Distance(new_position,!self.Target)
    if target_distance < new_target_distance && target_distance < 0.1f<m>  then 
      !self.Target
    else
      new_position
  static member VelocityRule(self:PhysicalEntity,dt:float32<s>) =
    let target_distance = Vector2<m>.Distance(!self.Position,!self.Target)
    if target_distance < 0.1f<m> then
      Vector2.Zero
    else
      (!self.Target - !self.Position).Normalized * !self.MaxSpeed

  /// Create a physical entity with an initial position and capable of a certain
  /// maximum velocity.
  static member Create(position:Vector2<m>,max_speed:float32<m/s>) =
    { 
      Position  = Rule.Create(position)
      Target    = var(position)
      Velocity  = Rule.Create(Vector2.Zero)
      MaxSpeed  = var max_speed
    }    

