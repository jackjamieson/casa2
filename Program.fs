// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Casanova
open Casanova.Core
open Casanova.Coroutines
open Casanova.Input
open Casanova.Utilities
open Casanova.Game
open Casanova.Math
open Casanova.Action
open Casanova.Drawing
open Casanova.StandardLibrary
open Casanova.StandardLibrary.Core

    type [<CasanovaWorld>] World = {
      p1 : p1
      p2 : p2
      ball: ball
      wall: wall
      wall2: wall2

    } with

     member world.ScreenSize = Vector2<m>.One * 500.0f
     member world.ScreenSize2 = Vector2<m>.One * -500.0f

    and [<CasanovaEntity>] p1 = {
         Position : Rule<Vector2<m>>
         Velocity : Rule<Vector2<m/s>>
         Sprite   : DrawableSprite
    } with
      static member PositionRule(world:World,self:p1,dt:float32<s>)=
      if(!self.Position).Y < world.ScreenSize2.Y then
        Vector2<m>((!self.Position).X,world.ScreenSize2.Y)
      elif (!self.Position).Y>world.ScreenSize.Y then
        Vector2<m>((!self.Position).X,world.ScreenSize.Y)
      else
        !self.Position + !self.Velocity*dt
    
      //static member Position'(self:p1,dt:float32<s>) = !self.Position + dt * !self.Velocity
      static member Velocity'(self:p1,dt:float32<s>) = !self.Velocity * 0.9f
      static member SpritePosition'(self:p1) = !self.Position * 1.0f<pixel/m>
      static member ScreenSize = Vector2<m>.One * 500.0f

    and [<CasanovaEntity>] p2 = {
         Position : Rule<Vector2<m>>
         Velocity : Rule<Vector2<m/s>>
         Sprite   : DrawableSprite
    } with
      static member PositionRule(world:World,self:p2,dt:float32<s>)=
      if(!self.Position).Y<world.ScreenSize2.Y then
        Vector2<m>((!self.Position).X,world.ScreenSize2.Y)
      elif (!self.Position).Y>world.ScreenSize.Y then
        Vector2<m>((!self.Position).X,world.ScreenSize.Y)
      else
        !self.Position + !self.Velocity*dt
      static member Velocity'(self:p2,dt:float32<s>) = !self.Velocity * 0.9f
      static member SpritePosition'(self:p2) = !self.Position * 1.0f<pixel/m>
      static member ScreenSize = Vector2<m>.One * 500.0f

  and [<CasanovaEntity>] ball = {
    Position: Rule<Vector2<m>>
    Life : Var<float32>
    Sprite : DrawableSprite
    Velocity: Rule<Vector2<m/s>>

  } with
    //static member Position'(self:ball,dt:float32<s>) = !self.Position + dt * Vector2<m/s>.UnitY * 300.0f
    static member SpritePosition'(self:ball) = !self.Position * 1.0f<pixel/m>
    static member VelocityRule(world:World,self:ball,dt:float32<s>) = 
    if (!self.Position).Y > world.ScreenSize.Y then
      Vector2<m/s>((!self.Velocity).X, -(!self.Velocity).Y)
    else
      !self.Velocity
        
    static member SpritePositionRule(self:ball,dt:float32<s>) = 
       !self.Position * Vector2<pixel/m>.One

    static member PositionRule(world:World,self:ball,dt:float32<s>) = 
    if (!self.Position).Y > world.ScreenSize.Y then
      Vector2<m>((!self.Position).X, world.ScreenSize.Y )
    else
      !self.Position + !self.Velocity * dt * 100.0f
   //ADDED THE WALLS HERE
   and [<CasanovaEntity>] wall = {
        Position : Rule<Vector2<m>>
        Sprite : DrawableSprite
    } with
    static member Position'(self:wall, dt:float32<s>) = !self.Position
    static member SpritePosition'(self:wall) = !self.Position * 1.0f<pixel/m>

   and [<CasanovaEntity>] wall2 = {
        Position : Rule<Vector2<m>>
        Sprite : DrawableSprite
    } with
    static member Position'(self:wall2, dt:float32<s>) = !self.Position
    static member SpritePosition'(self:wall2) = !self.Position * 1.0f<pixel/m>

 let start_game (game:StartGameArgs) =

  let world0 = 
    {
      p1 = 
        {
          Position = Rule.Create(Vector2<m>(-450.0f<m>, 0.0f<m>))
          Velocity = Rule.Create(Vector2<m/s>.Zero)
          Sprite   = DrawableSprite.Create(game.default_layer, Vector2<pixel>.One, Vector2<pixel>.One * 200.0f, @"p1")
        }

      p2 = 
        {
          Position = Rule.Create(Vector2<m>(450.0f<m>, 0.0f<m>))
          Velocity = Rule.Create(Vector2<m/s>.Zero)
          Sprite   = DrawableSprite.Create(game.default_layer, Vector2<pixel>.One, Vector2<pixel>.One * 200.0f, @"p2")
        }

      wall = 
        {
          Position = Rule.Create(Vector2<m>(480.0f<m>, 0.0f<m>))
          Sprite   = DrawableSprite.Create(game.default_layer, Vector2<pixel>.One, Vector2<pixel>.One * 1200.0f, @"blocker")
        }

      wall2 = 
        {
          Position = Rule.Create(Vector2<m>(-480.0f<m>, 0.0f<m>))
          Sprite   = DrawableSprite.Create(game.default_layer, Vector2<pixel>.One, Vector2<pixel>.One * 1200.0f, @"blocker")
        }

      ball = 
      {
          Position = Rule.Create(Vector2<m>(0.0f<m>, 0.0f<m>))
          Sprite   = DrawableSprite.Create(game.default_layer, Vector2<pixel>.One, Vector2<pixel>.One * 40.0f, @"ball")
          Life = Var.Create(1.0f)
          Velocity = Rule.Create(Vector2<m/s>(1.0f<m/s>, 0.0f<m/s>))
      }
    }
  let inline (!) x = immediate_lookup x
  let main = yield_
  let input = 
    [
      wait_key_press Keys.Escape  =>> game.quit()
      wait_key_press Keys.F9      =>> game.save "savefile"
      wait_key_press Keys.F10     =>> game.load "savefile"
      //wait_key_down Keys.D        => co{ world0.p1.Velocity := !world0.p1.Velocity + Vector2<m/s>.UnitX * 100.0f }
      //wait_key_down Keys.A        => co{ world0.p1.Velocity := !world0.p1.Velocity - Vector2<m/s>.UnitX * 100.0f }
      wait_key_down Keys.W        => co{ world0.p1.Velocity := !world0.p1.Velocity - Vector2<m/s>.UnitY * 100.0f }
      wait_key_down Keys.S        => co{ world0.p1.Velocity := !world0.p1.Velocity + Vector2<m/s>.UnitY * 100.0f }
      wait_key_down Keys.Up        => co{ world0.p2.Velocity := !world0.p2.Velocity - Vector2<m/s>.UnitY * 100.0f }
      wait_key_down Keys.Down        => co{ world0.p2.Velocity := !world0.p2.Velocity + Vector2<m/s>.UnitY * 100.0f }
    ]
  world0,main,input

[<EntryPoint>]
let main argv = 
  use game = Game.Create(start_game, 1024, 600, false)
  game.Run()
  0