// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

//Importing required libraries.
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

//First step is to create a world.  This holds all references to objects in the game.
    type [<CasanovaWorld>] World = {
      p1 : p1
      p2 : p2
      ball: ball
      wall: wall
      wall2: wall2
      middle: middle
      left: Rule<int> //A rule that uses the int data type.
      right: Rule<int>
      ScoreText: DrawableText

    } with//Describing the things inside of the world.


//The limits of the walls.
     member world.ScreenSize = Vector2<m>.One * 500.0f//Farthest right.
     member world.ScreenSize2 = Vector2<m>.One * -500.0f//Farthest left.
     static member ScreenSize3 = Vector2<m>.One * 500.0f
     static member ScoreTextStringRule(world:World,dt:float32<s>) = (string !world.left + "                     " + string !world.right)//Draws the score. 

    //Rules for the score on the left, adds 1 to the score if the ball goes past the invisible wall.  Adds 1 while it is between -1.25 and +2.05 distance from the wall.
     static member leftRule(world:World,dt:float32<s>) = 
        if   world.ball.Position.Value.X > (world.wall.Position.Value.X - 1.25f<m>) && world.ball.Position.Value.X < (world.wall.Position.Value.X + 2.05f<m>) then !world.left + 1
            else !world.left + 0

    //Rules for the score on the right, adds 1 to the score if the ball goes past the invisible wall.  Adds 1 while it is between +1.25 and -2.05 distance from the wall.
     static member rightRule(world:World,dt:float32<s>) = 
        if world.ball.Position.Value.X < world.wall2.Position.Value.X + 1.25f<m> && world.ball.Position.Value.X > (world.wall2.Position.Value.X - 2.05f<m>) then !world.right + 1
            else !world.right + 0



//Second step is to create the entities.

   //Paddle 1
   /////////////////////////////////////////////////////////////////
    and [<CasanovaEntity>] p1 = {//Create a new entity, the first paddle (on the left).
         Position : Rule<Vector2<m>> //Has a position.
         Velocity : Rule<Vector2<m/s>> //Has a velocity.
         Sprite   : DrawableSprite //Has a sprite.

    } with //Simple collision detection with the edges of the screen.
      static member PositionRule(world:World,self:p1,dt:float32<s>)=
        if(!self.Position).Y < world.ScreenSize2.Y then //if the Y axis position is below the screen, stop moving.
            Vector2<m>((!self.Position).X,world.ScreenSize2.Y)
                elif (!self.Position).Y>world.ScreenSize.Y then //if the Y axis position is above the screen, stop moving.
                    Vector2<m>((!self.Position).X,world.ScreenSize.Y)
                        else
                            !self.Position + !self.Velocity*dt //Otherwise keep moving.
    
      static member Velocity'(self:p1,dt:float32<s>) = !self.Velocity * 0.9f
      static member SpritePosition'(self:p1) = !self.Position * 1.0f<pixel/m>
      static member ScreenSize = Vector2<m>.One * 500.0f
 
    /////////////////////////////////////////////////////////////////


    //Paddle 2
    /////////////////////////////////////////////////////////////////
    and [<CasanovaEntity>] p2 = {//Same as above, but we made this one on the left.
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

    /////////////////////////////////////////////////////////////////


    //Ball entity
    /////////////////////////////////////////////////////////////////
    and [<CasanovaEntity>] ball = {
        Position: Rule<Vector2<m>>
        Sprite : DrawableSprite
        Velocity: Rule<Vector2<m/s>>

  } with
    static member SpritePosition'(self:ball) = !self.Position * 1.0f<pixel/m>

    //This ball code isn't formatted very well.
    //Handles the velocity of the moving ball.
    static member VelocityRule(world:World,self:ball,dt:float32<s>) = 
    if (!self.Position).Y > world.ScreenSize.Y then //Bounce off the top of the screen.
      Vector2<m/s>((!self.Velocity).X, -(!self.Velocity).Y)
    elif (!self.Position).Y < world.ScreenSize2.Y then //Bounce off the bottom of the screen.
      Vector2<m/s>((!self.Velocity).X, -(!self.Velocity).Y)

    elif (!self.Position).X > (world.p2.Position.Value.X) && ((!self.Position).Y < world.p2.Position.Value.Y + 100.0f<m> && (!self.Position).Y > world.p2.Position.Value.Y - 100.0f<m>) then
     Vector2<m/s>(-(!self.Velocity).X, (!self.Velocity).Y)//Bounce off the area of the paddle, had to create a range of bounce area with +100 and -100 (the length of the paddle).
    elif (!self.Position).X < world.p1.Position.Value.X && ((!self.Position).Y < world.p1.Position.Value.Y + 100.0f<m> && (!self.Position).Y > world.p1.Position.Value.Y - 100.0f<m>) then
     Vector2<m/s>(-(!self.Velocity).X, (!self.Velocity).Y)//Keep moving if it is not the area of the paddle.

    else
      !self.Velocity //Otherwise keep moving.

    static member SpritePositionRule(self:ball,dt:float32<s>) = 
       !self.Position * Vector2<pixel/m>.One

    //Positions need to be updated to match velocities when it changes.
    static member PositionRule(world:World,self:ball,dt:float32<s>) = 
    if (!self.Position).Y > world.ScreenSize.Y then
      Vector2<m>((!self.Position).X, world.ScreenSize.Y )
    elif (!self.Position).Y < world.ScreenSize2.Y then
      Vector2<m>((!self.Position).X, world.ScreenSize2.Y )
    elif (!self.Position).X < world.ScreenSize2.X then
      Vector2<m>(0.0f<m>, 0.0f<m>) //Restting the ball when it passes the paddle.
    elif (!self.Position).X > world.ScreenSize.X then
      Vector2<m>(0.0f<m>, 0.0f<m>) //Restting the ball when it passes the paddle.

    elif (!self.Position).X > world.p2.Position.Value.X && ((!self.Position).Y > world.p2.Position.Value.Y + 100.0f<m> && (!self.Position).Y < world.p2.Position.Value.Y - 100.0f<m>)then
     Vector2<m>(self.Position.Value.X, self.Position.Value.Y) // New ball position when bouncing off paddle.
    elif (!self.Position).X > world.p2.Position.Value.X && ((!self.Position).Y < world.p2.Position.Value.Y + 100.0f<m> && (!self.Position).Y > world.p2.Position.Value.Y - 100.0f<m>)then
     Vector2<m>(world.p2.Position.Value.X, self.Position.Value.Y)

    elif (!self.Position).X < world.p1.Position.Value.X && ((!self.Position).Y > world.p1.Position.Value.Y + 100.0f<m> && (!self.Position).Y < world.p1.Position.Value.Y - 100.0f<m>)then
     Vector2<m>(self.Position.Value.X, self.Position.Value.Y)// Letting the ball go through the invisible wall.
    elif (!self.Position).X < world.p1.Position.Value.X && ((!self.Position).Y < world.p1.Position.Value.Y + 100.0f<m> && (!self.Position).Y > world.p1.Position.Value.Y - 100.0f<m>)then
     Vector2<m>(world.p1.Position.Value.X, self.Position.Value.Y)

    else
      !self.Position + !self.Velocity * dt * 150.0f

    /////////////////////////////////////////////////////////////////


    //Wall entities
    /////////////////////////////////////////////////////////////////
   and [<CasanovaEntity>] wall = { //Invisible wall on the left
        Position : Rule<Vector2<m>>
        Sprite : DrawableSprite

    } with
    static member Position'(self:wall, dt:float32<s>) = !self.Position
    static member SpritePosition'(self:wall) = !self.Position * 1.0f<pixel/m>

   and [<CasanovaEntity>] wall2 = { //Invisible wall on the right.
        Position : Rule<Vector2<m>>
        Sprite : DrawableSprite
    } with
    static member Position'(self:wall2, dt:float32<s>) = !self.Position
    static member SpritePosition'(self:wall2) = !self.Position * 1.0f<pixel/m>

   and [<CasanovaEntity>] middle = { //Middle wall, does nothing it is for show.
        Position : Rule<Vector2<m>>
        Sprite : DrawableSprite

    } with
    static member Position'(self:middle, dt:float32<s>) = !self.Position
    static member SpritePosition'(self:middle) = !self.Position * 1.0f<pixel/m>


//Third step is to define the entities in the world.

 let start_game (game:StartGameArgs) =

  let world0 = 
    {
      left = Rule.Create(0)//Initialize left score to 0.
      right = Rule.Create(0)//Same on the right.
      //Place the score text.
      ScoreText = DrawableText.Create(game.default_layer, @"arial.xnb", Vector2<m>(-World.ScreenSize3.X + 235.0f<m>, -World.ScreenSize3.Y + 50.0f<m>) * 1.0f<pixel/m>, Vector2<pixel>.Zero, "0", Color.White, Vector2<pixel>.One * 100.f)

      p1 = 
        {
          Position = Rule.Create(Vector2<m>(-450.0f<m>, 0.0f<m>)) //Defines initial location.
          Velocity = Rule.Create(Vector2<m/s>.Zero) //Defines initial velocity.
          Sprite   = DrawableSprite.Create(game.default_layer, Vector2<pixel>.One, Vector2<pixel>.One * 200.0f, @"p1") //Defines the sprite and the size.
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

      middle = 
        {
          Position = Rule.Create(Vector2<m>(0.0f<m>, 0.0f<m>))
          Sprite   = DrawableSprite.Create(game.default_layer, Vector2<pixel>.One, Vector2<pixel>.One * 800.0f, @"middle")
        }

      ball = 
      {
          Position = Rule.Create(Vector2<m>(0.0f<m>, 0.0f<m>))
          Sprite   = DrawableSprite.Create(game.default_layer, Vector2<pixel>.One, Vector2<pixel>.One * 22.0f, @"ball")
          Velocity = Rule.Create(Vector2<m/s>(1.3f<m/s>, 2.0f<m/s>))
      }
    }
  let inline (!) x = immediate_lookup x
  let main = yield_
  let input = 
    [
      //Settings keys.
      wait_key_press Keys.Escape  =>> game.quit()
      wait_key_press Keys.F9      =>> game.save "savefile"
      wait_key_press Keys.F10     =>> game.load "savefile"

      //Keys to move the paddles.
      wait_key_down Keys.W        => co{ world0.p1.Velocity := !world0.p1.Velocity - Vector2<m/s>.UnitY * 100.0f }
      wait_key_down Keys.S        => co{ world0.p1.Velocity := !world0.p1.Velocity + Vector2<m/s>.UnitY * 100.0f }
      wait_key_down Keys.Up        => co{ world0.p2.Velocity := !world0.p2.Velocity - Vector2<m/s>.UnitY * 100.0f }
      wait_key_down Keys.Down        => co{ world0.p2.Velocity := !world0.p2.Velocity + Vector2<m/s>.UnitY * 100.0f }
    ]
  world0,main,input

[<EntryPoint>]
let main argv = 
  use game = Game.Create(start_game, 1024, 600, false, "Pong in Casanova!")//Title bar and true/false for full screen.
  game.Run()
  0