module PauseMenu

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

// The Menu contains the menu content text, the header text and the layer associated.
[<CasanovaEntity>]
type Menu = {
    // The layer used to draw the background
    BackgroundLayer : SpriteLayer
    // The layer used to draw the menu
    TextLayer       : SpriteLayer
    // The background image of this menu
    Background : DrawableSprite
    // The drawable text with the content
    Text      : DrawableText
  }


let start_menu game_name background start_actual_game (args : StartGameArgs) = 
  // The layer
  let layer = SpriteLayer.Create(args.graphics_device, args.content)
  let background_layer = SpriteLayer.Create(args.graphics_device, args.content)
  // The menu is created with the layer and with the two drawable text
  let world = 
    {
      TextLayer = layer
      BackgroundLayer = background_layer
      Background = 
        DrawableSprite.Create(
          background_layer,
          -Vector2<_>.One * 500.f,
          Vector2<_>.One * 4000.f, 
          0.f,
          Vector2<_>.One * 0.5f,
          @"UI/white_pixel",
          Color.FromNonPremultiplied(0,0,0,150))
      Text = 
        DrawableText.Create(
          layer,
          "Arial",
          -Vector2<_>.UnitX * 375.f,
          Vector2<_>.Zero,
          "Save (F9)\nLoad (F10)\nBack to main menu (Q)",
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
  // when the user presses F9 key the game will be saved
  // when the user presses F10 key the game will be loaded
  // when the user presses N key a new game wil start
  // when the user presses Q key it ask quit confirmation
  // when the user presses Escape key if there is a game it will be resumed, if not it ask quit confirmation;
  let input = 
    [
      wait_key_press Keys.F9 =>> args.save game_name
      wait_key_press Keys.F10 =>> args.load game_name
      wait_key_press Keys.Q =>> co{ do args.SetStack (MainMenu.start_menu game_name background start_actual_game) }
      wait_key_press Keys.Escape =>> co{ do args.PopStack() }
    ]
  world, main, input




