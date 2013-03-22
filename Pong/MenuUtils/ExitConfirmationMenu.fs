module ExitConfirmationMenu

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Graphics
open Microsoft.Xna.Framework.Input
open Casanova
open Casanova.Core
open Casanova.Coroutines
open Casanova.Utilities
open Casanova.Math
open Casanova.Game
open Casanova.Drawing
open Casanova.StandardLibrary
open Casanova.StandardLibrary.Core

// The Menu contains the text to draw and the associated layer.
[<CasanovaEntity>]
type Menu = {
    // The layer used to draw the menu
    Layer     : SpriteLayer
    // The drawable text to be displayed
    Text      : DrawableText
  }


let start_menu (args : StartGameArgs) = 
  // The layer
  let layer = SpriteLayer.Create(args.graphics_device, args.content)
  // The menu is created with the layer and with the drawable text
  let world = 
    {
      Layer = layer
      Text = 
        DrawableText.Create(
          layer,
          "Arial",
          -Vector2<_>.UnitX * 490.f,
          Vector2<_>.UnitY * 50.f,
          "Quit?\nYes (Y)\nNo (N)",
          Color.White,
          Vector2<_>.One * 200.f)
    }

  // The (!) operator which in rules is used to access the current value, in scripts 
  // will access the next value; this way scripts act as completely imperative
  // coroutines
  let inline (!) x = immediate_lookup x
  
  // The main script does nothing.
  let main = 
    co{ do! yield_} |> repeat_

  // Three input scripts: 
  // when the user presses the Y key the menu will invoke exit method that closes the application;
  // when the user presses the N or Escape key the menu will be pop the stack removing itself from it;
  let input = 
    [
      wait_key_press Keys.Y =>> co{ do args.exit() };
      wait_key_press Keys.N =>> co{ do args.pop_stack() }
      wait_key_press Keys.Escape =>> co{ do args.pop_stack() }
    ]
  world, main, input


