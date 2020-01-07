namespace Asteroids

open Browser.Types

[<Struct>]
type Keyboard =
  {
    UpArrow : bool
    LeftArrow : bool
    DownArrow : bool
    RightArrow : bool
    Space : bool
  }

module Keyboard =

  let zero =
    {
      UpArrow = false
      LeftArrow = false
      DownArrow = false
      RightArrow = false
      Space = false
    }

type Game<'T> =
  {
    Init : Unit -> 'T
    ApplyInput : Keyboard -> 'T -> 'T
    Update : 'T -> 'T
    Render : CanvasRenderingContext2D -> 'T -> Unit
  }
