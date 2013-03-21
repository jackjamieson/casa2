module Casanova.Drawing

open Microsoft.Xna.Framework
open Microsoft.Xna.Framework.Content
open Microsoft.Xna.Framework.Graphics
open Casanova
open Casanova.Core
open Casanova.Math
open Casanova.Utilities

[<Measure>]
type pixel

let inline private to_xna (v:Vector2<pixel>) = v.ToXNA
let inline private (<==) x y = (^a: (static member Apply: ^a * ^b -> Unit) (x,y)) 

/// Wrapper type used to serialize BlendState. Which mimix the behaviour of the first BlendState itself.
type BlendState = 
  [<DefaultValue>]
  val mutable private index : int

  static member private vals = 
                              [| 
                                Microsoft.Xna.Framework.Graphics.BlendState.Additive; 
                                Microsoft.Xna.Framework.Graphics.BlendState.AlphaBlend; 
                                Microsoft.Xna.Framework.Graphics.BlendState.NonPremultiplied; 
                                Microsoft.Xna.Framework.Graphics.BlendState.Opaque 
                              |]

  private new () =
    { }
  private new (i : int) as this =
      { }
      then
        this.index <- i

  member internal this._get() : Microsoft.Xna.Framework.Graphics.BlendState = BlendState.vals.[this.index]

  new (
      info : System.Runtime.Serialization.SerializationInfo, 
      context : System.Runtime.Serialization.StreamingContext) as this =
       { }
          then 
            do this.index <- info.GetValue("index", typeof<int>) :?> int

  interface System.Runtime.Serialization.ISerializable
    with member this.GetObjectData (info : System.Runtime.Serialization.SerializationInfo, context : System.Runtime.Serialization.StreamingContext) : unit =          
          do info.AddValue("index", this.index, typeof<int>)
          
  static member Additive =
          new BlendState(0)
  static member AlphaBlend =
          new BlendState(1)
  static member NonPremultiplied =
          new BlendState(2)
  static member Opaque =
          new BlendState(3)    

/// Wrapper type used to serialize GraphicsDevice. Which mimix the behaviour of the first GraphicsDevice itself.
type GraphicsDevice = 
  [<DefaultValue; System.NonSerialized>] 
  val mutable private g_d : Microsoft.Xna.Framework.Graphics.GraphicsDevice
  new (graphics_d : Microsoft.Xna.Framework.Graphics.GraphicsDevice) as this =
    { }
      then
      this.g_d <- graphics_d
  member internal this.Reload (graphics : Microsoft.Xna.Framework.Graphics.GraphicsDevice) =
    this.g_d <- graphics
  member internal this.Value with get () = this.g_d
  member this.Viewport with get () = this.g_d.Viewport
type ContentManager = Microsoft.Xna.Framework.Content.ContentManager
type Texture2D = Microsoft.Xna.Framework.Graphics.Texture2D
type Color = Microsoft.Xna.Framework.Color

/// A SpriteLayer stores all the 2D drawables that are to be drawn together with a certain transform and with a certain alpha-blending state.
[<CasanovaDrawable>]
type SpriteLayer = {
    AlphaBlend        : Rule<BlendState>
    AlphaTest         : Rule<bool>
    AllLines          : RuleTable<DrawableLine>
    AllTexts          : RuleTable<DrawableText>
    AllShadowedTexts  : RuleTable<ShadowedText>
    AllSprites        : RuleTable<DrawableSprite>
    [<System.NonSerialized>]
    mutable Content           : ContentManager
    [<System.NonSerialized>]
    mutable SpriteBatch       : SpriteBatch
    Transform         : Rule<Matrix>
    NetworkingContext : NetworkingContext
  }
  with
  /// Internal use only: called after deserialization to restore non-serializable attributes to itself and to registered entities.
  member this.Reload (content : ContentManager, graphics_device : GraphicsDevice) = 
    this.Content <- content
    this.SpriteBatch <- new SpriteBatch(graphics_device.Value)
    for vals in this.AllLines.values do
      for l in vals do
        do l.Reload()
    for vals in this.AllShadowedTexts.values do
      for r in vals do
        do r.Reload()
    for vals in this.AllSprites.values do
      for s in vals do
        do s.Reload()
    for vals in this.AllTexts.values do
      for t in vals do
        do t.Reload()
  /// Unproject a vector from screen-space to unnormalized screen-space.
  member this.Unproject<[<Measure>] 'u>(pos : Vector2<pixel>) =
    let screen_size = Vector2(float32 this.SpriteBatch.GraphicsDevice.Viewport.Width, 
                              float32 this.SpriteBatch.GraphicsDevice.Viewport.Height)
    let transform = !this.Transform  * Matrix.CreateTranslation(0.5f * screen_size.X, 0.5f * screen_size.Y, 0.0f)
    let transform_inverse = Matrix.Invert transform
    Vector2<'u>(Vector2.Transform(pos.ToXNA, transform_inverse) * 1000.0f / screen_size)

  /// Internal use only: copy all current values from the current to the next state of each rule.
  member this.IdenticalRule() =
    do this.AlphaBlend          <== !this.AlphaBlend
    do this.AlphaTest           <== !this.AlphaTest
    do this.AllTexts            <== !this.AllTexts
    do this.AllShadowedTexts    <== !this.AllShadowedTexts
    do this.AllLines            <== !this.AllLines
    do this.AllSprites          <== !this.AllSprites
    do this.Transform           <== !this.Transform

  /// Internal use only: clear the next values for the sprites, texts, and lines rules.
  member this.Clear() = 
    do this.IdenticalRule()
    do this.AllSprites.Clear()
    do this.AllTexts.Clear()    
    do this.AllShadowedTexts.Clear()    
    do this.AllLines.Clear()    

  /// Internal use only: register to its own container. Since the SpriteLayer has no containers, registration
  /// does nothing.
  member this.Register() = ()

  /// Internal use only: draw all sprites, lines, and texts scaled so that the center of the screen is (0,0).
  member this.Draw() =
    let screen_size = Vector2(float32 this.SpriteBatch.GraphicsDevice.Viewport.Width, 
                              float32 this.SpriteBatch.GraphicsDevice.Viewport.Height)
    let transform = !this.Transform * Matrix.CreateTranslation(0.5f * screen_size.X, 0.5f * screen_size.Y, 0.0f)

    do this.SpriteBatch.Begin(SpriteSortMode.Immediate, (!this.AlphaBlend)._get(), null, null, null, null, transform)

    for line in this.AllLines do
      let texture_size = line.Sprite.Value.Size
      let source_pos = line.Source.Value.ToXNA / 1000.0f * screen_size
      let dest_pos = line.Dest.Value.ToXNA / 1000.0f * screen_size
      let position = source_pos
      let origin = Vector2<pixel>(0.0f<pixel>, 0.5f<pixel>)  |> to_xna
      let rotation = 
        let dest_source = dest_pos - source_pos
        atan2 dest_source.Y dest_source.X
      let scale   = Vector2((Vector2.Distance(dest_pos,source_pos)) / texture_size.X, (screen_size.Y * line.Thickness.Value / (1000.0f * texture_size.Y)))
      do this.SpriteBatch.Draw(!line.Sprite, position, System.Nullable<Rectangle>(), !line.Color, rotation, origin, scale, SpriteEffects.None, 0.f)

    for sprite in this.AllSprites do
      let texture_size = sprite.Sprite.Value.Size
      let actual_size = screen_size * (!sprite.Size / 1000.f<_>).ToXNA
      let scale = (!sprite.Scale).ToXNA * actual_size / texture_size
      let origin = sprite.Origin.Value.ToXNA * texture_size
      let position = (!sprite.Position |> to_xna) / 1000.0f * screen_size
      if !sprite.Stretch then 
        do this.SpriteBatch.Draw(!sprite.Sprite, position, System.Nullable<Rectangle>(), !sprite.Color, !sprite.Rotation, origin, scale, SpriteEffects.None, 0.f)
      else 
        let scale = min scale.X scale.Y 
        do this.SpriteBatch.Draw(!sprite.Sprite, position, System.Nullable<Rectangle>(), !sprite.Color, !sprite.Rotation, origin, scale, SpriteEffects.None, 0.f)

    for text in this.AllTexts do
      let font = this.Content.Load<SpriteFont>(!text.Font)
      let texture_size = font.MeasureString(!text.String)
      let actual_size = screen_size * (!text.Size / 1000.f).ToXNA
      let scale = actual_size / (min texture_size.X texture_size.Y)
      let scale = min scale.X scale.Y      
      let origin = text.Origin.Value |> to_xna
      let position = (!text.Position |> to_xna) / 1000.0f * screen_size
      do this.SpriteBatch.DrawString(font, !text.String, position, !text.Color, 0.0f, origin, scale, SpriteEffects.None, 0.0f)

    for text in this.AllShadowedTexts do
      let font = this.Content.Load<SpriteFont>(!text.Font)
      let texture_size = font.MeasureString(!text.String)
      let actual_size = screen_size * (!text.Size / 1000.f).ToXNA
      let scale = actual_size / (min texture_size.X texture_size.Y)
      let scale = min scale.X scale.Y      
      let origin = text.Origin.Value |> to_xna
      let position = (!text.Position |> to_xna) / 1000.0f * screen_size
      let shadow_position = ((!text.Position + !text.ShadowOffset) |> to_xna) / 1000.0f * screen_size
      do this.SpriteBatch.DrawString(font, !text.String, shadow_position, !text.ShadowColor, 0.0f, origin, scale, SpriteEffects.None, 0.0f)
      do this.SpriteBatch.DrawString(font, !text.String, position, !text.Color, 0.0f, origin, scale, SpriteEffects.None, 0.0f)

    do this.SpriteBatch.End()

  /// Initialize a sprite layer with a certain graphics device, a certain content, a certain alpha-blending state,
  /// and an initial transform.
  static member Create(graphicsDevice : GraphicsDevice, content : ContentManager, alphaBlend : BlendState, transform : Matrix) =
    {
      AlphaBlend        = Rule<BlendState>.Create(fun () -> alphaBlend)
      AlphaTest         = Rule.Create(true)
      AllLines          = RuleTable.Empty
      AllTexts          = RuleTable.Empty
      AllShadowedTexts  = RuleTable.Empty
      AllSprites        = RuleTable.Empty
      Content           = content
      SpriteBatch       = new SpriteBatch(graphicsDevice.Value)
      Transform         = Rule.Create(transform)
      NetworkingContext = NetworkingContext()
    }
  /// Initialize a sprite layer with a certain graphics device, a certain content, and a certain alpha-blending state.
  static member Create(graphicsDevice : GraphicsDevice, content : ContentManager, alphaBlend : BlendState) =
    SpriteLayer.Create(graphicsDevice, content, alphaBlend, Matrix.Identity)

  /// Initialize a sprite layer with a certain graphics device, a certain content, and the default alpha-blending state of non premultiplied blending.
  static member Create(graphicsDevice : GraphicsDevice, content : ContentManager) =
    SpriteLayer.Create(graphicsDevice, content, BlendState.NonPremultiplied, Matrix.Identity)

  /// Initialize a sprite layer with a certain graphics device, a certain content, 
  /// a specified initial transform, and the default alpha-blending state of non premultiplied blending.
  static member Create(graphicsDevice : GraphicsDevice, content : ContentManager, transform : Matrix) =
        SpriteLayer.Create(graphicsDevice, content, BlendState.NonPremultiplied, transform)


/// A DrawableSprite contains a picture to be drawn and all its parameters: sprite layer it is drawn with, path of its texture, color, 
/// current texture, origin (center of its transforms such as scale and rotation), position, rotation, size, scale, and whether or
/// not to stretch the texture when scaling it.
and [<CasanovaDrawable>] DrawableSprite = {
    Color             : Rule<Color>
    Layer             : Rule<SpriteLayer>
    [<System.NonSerialized>]
    mutable Sprite            : Rule<Texture2D>
    Path              : Rule<string>
    Origin            : Rule<Vector2<pixel>>
    Position          : Rule<Vector2<pixel>>
    Rotation          : Rule<float32>
    Size              : Rule<Vector2<pixel>>
    Scale             : Rule<Vector2<1>>
    Stretch           : Rule<bool>
    NetworkingContext : NetworkingContext
  }
  with
  /// Internal use only: called after deserialization, restore non-serializable attributes.
  member this.Reload() = 
    this.Sprite <- Rule.Create (this.Layer.Value.Content.Load<Texture2D>(!this.Path))
  /// Internal use only: register the sprite into its containing layer.
  member this.Register() =
    this.Layer.Value.AllSprites.Add(this)
  /// Internal use only: copy all current values into the corresponding next values.
  member this.Clear() =
    do this.Color     <== !this.Color
    do this.Layer     <== !this.Layer
    do this.Sprite    <== (!this.Layer).Content.Load<Texture2D>(!this.Path)
    do this.Path      <== !this.Path
    do this.Origin    <== !this.Origin
    do this.Position  <== !this.Position
    do this.Rotation  <== !this.Rotation
    do this.Size      <== !this.Size
    do this.Scale     <== !this.Scale
    do this.Stretch   <== !this.Stretch
  /// Initialize a drawable sprite by specifying its layer, its position, its size, its rotation, 
  /// its origin of transformation, its path, its color, and whether or not to stretch it.
  static member Create(layer : SpriteLayer, 
                       position : Vector2<pixel>, 
                       size : Vector2<pixel>, 
                       rotation : float32, 
                       origin : Vector2<pixel>, 
                       path : string, 
                       color : Color,
                       stretch : bool) =
    let sprite = layer.Content.Load<Texture2D>(path)
    {
      Color             = Rule.Create(color)
      Layer             = Rule<SpriteLayer>.Create(fun () -> layer)
      Sprite            = Rule<Texture2D>.Create(fun () -> sprite)
      Path              = Rule<string>.Create(fun () -> path)
      Origin            = Rule.Create(origin)
      Size              = Rule.Create(size)
      Scale             = Rule.Create(Vector2<1>.One)
      Rotation          = Rule.Create(rotation)
      Position          = Rule.Create(position)
      Stretch           = Rule.Create(stretch)
      NetworkingContext = NetworkingContext()
    }

  /// Initialize a drawable sprite by specifying its layer, its position, its size, its rotation, 
  /// its origin of transformation, its path, and its color. Stretching is disabled.
  static member Create(layer : SpriteLayer, 
                       position : Vector2<pixel>, 
                       size : Vector2<pixel>, 
                       rotation : float32, 
                       origin : Vector2<pixel>, 
                       path : string, 
                       color : Color) = DrawableSprite.Create(layer, position, size, rotation, origin, path, color, false)
  /// Initialize a drawable sprite by specifying its layer, its position, its size, its path, and its color. 
  /// Origin is center of texture, there is no rotation, and stretching is disabled.
  static member Create(layer : SpriteLayer, 
                       position : Vector2<pixel>, 
                       size : Vector2<pixel>, 
                       path : string,
                       color : Color) = DrawableSprite.Create(layer, position, size, 0.0f, Vector2<pixel>.One * 0.5f, path, color)
  /// Initialize a drawable sprite by specifying its layer, its position, its size, its path, and its color. 
  /// Origin is center of texture, there is no rotation, color is white, and stretching is disabled.
  static member Create(layer : SpriteLayer, 
                       position : Vector2<pixel>, 
                       size : Vector2<pixel>, 
                       path : string) = DrawableSprite.Create(layer, position, size, path, Color.White)


/// A DrawableLine contains a texture to be drawn between two points, and all its parameters: the sprite layer it is drawn with, the path of its texture, its color, 
/// its current texture, its source position, its destination, and its thickness.
and [<CasanovaDrawable>] DrawableLine = {
    Layer             : Rule<SpriteLayer>
    [<System.NonSerialized>]
    mutable Sprite            : Rule<Texture2D>
    Path              : Rule<string>
    Source            : Rule<Vector2<pixel>>
    Dest              : Rule<Vector2<pixel>>
    Thickness         : Rule<float32>
    Color             : Rule<Color>
    NetworkingContext : NetworkingContext
  }
  with
  /// Internal use only: called after deserialization, restore non-serializable attributes.
  member this.Reload() = 
    this.Sprite <- Rule.Create (this.Layer.Value.Content.Load<Texture2D>(!this.Path))
  /// Internal use only: register the sprite into its containing layer.
  member this.Register() =
    this.Layer.Value.AllLines.Add (this)
  /// Internal use only: copy all current values into the corresponding next values.
  member this.Clear() =
    do this.Color     <== !this.Color
    do this.Layer     <== !this.Layer
    do this.Sprite    <== (!this.Layer).Content.Load<Texture2D>(!this.Path)
    do this.Source    <== !this.Source
    do this.Dest      <== !this.Dest
    do this.Thickness <== !this.Thickness
  /// Initialize a line with its layer, a source and a destination point, 
  /// the path of its texture, its color, and its thickness.
  static member Create( layer : SpriteLayer, 
                        source : Vector2<_>, 
                        dest : Vector2<_>,                       
                        path : string, 
                        color : Color,
                        thickness : float32) : DrawableLine =
    let sprite = layer.Content.Load<Texture2D>(path)
    {
      Color             = Rule.Create(color)
      Layer             = Rule<SpriteLayer>.Create(fun () -> layer)
      Sprite            = Rule<Texture2D>.Create(fun () -> sprite)
      Path              = Rule<string>.Create(fun () -> path)
      Source            = Rule.Create(source)
      Dest              = Rule.Create(dest)
      Thickness         = Rule.Create(thickness)
      NetworkingContext = NetworkingContext()
    }


/// A DrawableText contains a string to be drawn with a certain font, and all its parameters: sprite layer it is drawn with, 
/// path of its font, string to draw, color, position, and size.
and [<CasanovaDrawable>] DrawableText = {
    Color             : Rule<Color>
    Font              : Rule<string>
    Layer             : Rule<SpriteLayer>
    Position          : Rule<Vector2<pixel>>
    Origin            : Rule<Vector2<pixel>>
    String            : Rule<string>
    Size              : Rule<Vector2<pixel>>
    NetworkingContext : NetworkingContext
  }
  with
  /// Internal use only: called after deserialization, restore non-serializable attributes.
  member this.Reload() = 
    ()
  /// Internal use only: register the sprite into its containing layer.
  member this.Register() =
    this.Layer.Value.AllTexts.Add (this)
  /// Internal use only: copy all current values into the corresponding next values.
  member this.Clear() =
    do this.Color     <== !this.Color
    do this.Font      <== !this.Font
    do this.Layer     <== !this.Layer
    do this.Position  <== !this.Position
    do this.String    <== !this.String
    do this.Size      <== !this.Size
  /// Create a drawable text with its layer, its font, its position, its string, its color, and its size.
  static member Create(layer : SpriteLayer, font : string, position : Vector2<pixel>, origin : Vector2<pixel>, string : string, color : Color, size : Vector2<pixel>) =
    {
      Font              = Rule<string>.Create(fun () -> font)
      Position          = Rule.Create(position)
      Origin            = Rule.Create(origin)
      String            = Rule<string>.Create(fun () -> string)
      Color             = Rule.Create(color)
      Layer             = Rule<SpriteLayer>.Create(fun () -> layer)
      Size              = Rule.Create(size)
      NetworkingContext = NetworkingContext()
    }
  /// Create a drawable text with its layer, its font, its position, its string, and its size. Color is white.
  static member Create(layer : SpriteLayer, font : string, position : Vector2<pixel>, string : string, size : Vector2<pixel>) =
    DrawableText.Create(layer, font, position, Vector2<pixel>.Zero, string, Color.White, size)
  /// Create a drawable text with its layer, its position, its string, and its size. Color is white and font is arial.
  static member Create(layer : SpriteLayer, position : Vector2<pixel>, string : string, size : Vector2<pixel>) =
    DrawableText.Create(layer, "arial", position, Vector2<pixel>.Zero, string, Color.White, size)
  /// Create a drawable text with its layer, its position, and its size. Color is white, string is "", and font is arial.
  static member Create(layer : SpriteLayer, position : Vector2<pixel>, size : Vector2<pixel>) =
    DrawableText.Create(layer, "arial", position, Vector2<pixel>.Zero, "", Color.White, size)
  /// Create a drawable text with its layer, its position, its size, and its color. String is "", and font is arial.
  static member Create(layer : SpriteLayer, position : Vector2<pixel>, size : Vector2<pixel>, color) =
    DrawableText.Create(layer, "arial", position, Vector2<pixel>.Zero, "", color, size)
  /// Create a drawable text with its layer, its position, its text, its size, and its color. String is "", and font is arial.
  static member Create(layer : SpriteLayer, position : Vector2<pixel>, text:string, size : Vector2<pixel>, color) =
    DrawableText.Create(layer, "arial", position, Vector2<pixel>.Zero, text, color, size)


/// Shadowed text is text that is drawn twice: once for a shadow, and once in the foreground.
and [<CasanovaDrawable>] ShadowedText = { 
    Color             : Rule<Color>
    Font              : Rule<string>
    Layer             : Rule<SpriteLayer>
    Position          : Rule<Vector2<pixel>>
    Origin            : Rule<Vector2<pixel>>
    String            : Rule<string>
    Size              : Rule<Vector2<pixel>>
    ShadowOffset      : Rule<Vector2<pixel>>
    ShadowColor       : Rule<Color>
    NetworkingContext : NetworkingContext
  } with
    /// Internal use only: called after deserialization, restore non-serializable attributes.
    member this.Reload() = 
      ()
    /// Internal use only: register the sprite into its containing layer.
    member this.Register() =
      this.Layer.Value.AllShadowedTexts.Add(this)
    /// Internal use only: copy all current values into the corresponding next values.
    member this.Clear() =
      do this.Color     <== !this.Color
      do this.Font      <== !this.Font
      do this.Layer     <== !this.Layer
      do this.Position  <== !this.Position
      do this.String    <== !this.String
      do this.Size      <== !this.Size

    /// A shadowed text is created with the same parameters of a drawable text, plus the offset and color of the shadow.
    static member Create(layer : SpriteLayer, position : Vector2<pixel>, string, shadow_offset : Vector2<pixel>, size : Vector2<pixel>, color : Color, shadow_color : Color) =
      {
        Font              = Rule<string>.Create(fun () -> "arial")
        Position          = Rule.Create(position)
        Origin            = Rule.Create(Vector2<pixel>.Zero)
        String            = Rule<string>.Create(fun () -> string)
        Color             = Rule.Create(color)
        ShadowColor       = Rule.Create(shadow_color)
        ShadowOffset      = Rule.Create(shadow_offset)
        Layer             = Rule<SpriteLayer>.Create(fun () -> layer)
        Size              = Rule.Create(size)
        NetworkingContext = NetworkingContext()
      }
