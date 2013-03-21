/// Provides input helper functions to read keyboard and mouse states.
module Casanova.Input

open Microsoft.Xna.Framework.Input

type Keys = Microsoft.Xna.Framework.Input.Keys

/// Checks if a key (from the Keys enumeration) is currently pressed.
let is_key_down k = Keyboard.GetState().IsKeyDown k

/// Checks if a key (from the Keys enumeration) is currently not pressed.
let is_key_up k = Keyboard.GetState().IsKeyUp k

/// Gets the current mouse position.
let mouse_position() = let m = Mouse.GetState() in m.X,m.Y

/// Checks if the left mouse button is currently pressed.
let is_mouse_left_down() = Mouse.GetState().LeftButton = ButtonState.Pressed

/// Checks if the left mouse button is currently released.
let is_mouse_left_up() = Mouse.GetState().LeftButton = ButtonState.Released

/// Checks if the left mouse button is currently pressed.
let is_mouse_right_down() = Mouse.GetState().RightButton = ButtonState.Pressed

/// Checks if the left mouse button is currently released.
let is_mouse_right_up() = Mouse.GetState().RightButton = ButtonState.Released

let mouse_scroll_wheel_value() = Mouse.GetState().ScrollWheelValue