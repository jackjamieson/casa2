module MainMenu

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

// The Menu contains the menu content text, the background and the layer associated.
[<CasanovaEntity>]
type Menu = {
    // The layer used to draw the background
    BackgroundLayer : SpriteLayer
    // The layer used to draw the menu
    TextLayer     : SpriteLayer
    // The background image of this menu
    Background : DrawableSprite
    // The drawable text with the content
    Text      : DrawableText
  }


let start_menu game_name background start_actual_game (args : StartGameArgs) = 
  // The layer for the text
  let layer = SpriteLayer.Create(args.graphics_device, args.content)
  // The layer for the background
  let background_layer = SpriteLayer.Create(args.graphics_device, args.content)
  // The menu is created with the layer and with the two drawable text
  let world = 
    {
      TextLayer = layer
      BackgroundLayer = background_layer
      Background = 
        DrawableSprite.Create(background_layer, Vector2<pixel>.Zero, Vector2<pixel>.One * 2000.0f, background)
      Text = 
        DrawableText.Create(
          layer,
          "Arial",
          -Vector2<_>.UnitX * 350.f,
          Vector2<_>.One * 50.f,
          "Start new game (N)\nLoad (F10)\nQuit (Q)",
          Color.White,
          Vector2<_>.One * 150.f)
    }

  // The (!) operator which in rules is used to access the current value, in scripts 
  // will access the next value; this way scripts act as completely imperative
  // coroutines
  let inline (!) x = immediate_lookup x
  
  // The main script does nothing.
  let main = 
    co{ do! yield_}

  // Five input scripts: 
  // when the user presses F10 key the game will be loaded
  // when the user presses N key a new game wil start
  // when the user presses Q key it quit
  let input = 
    [
      wait_key_press Keys.F10 =>> args.load game_name
      wait_key_press Keys.N =>> co{ do args.SetStack start_actual_game}
      wait_key_press Keys.Q =>> co{ do args.exit() }
    ]
  world, main, input