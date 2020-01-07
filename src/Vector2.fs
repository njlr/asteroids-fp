namespace Asteroids

[<Struct>]
type Vector2<'T> =
  {
    X : 'T
    Y : 'T
  }
  with
    static member inline (+) (u, v) =
      {
        X = u.X + v.X
        Y = u.Y + v.Y
      }

    static member inline (-) (u, v) =
      {
        X = u.X - v.X
        Y = u.Y - v.Y
      }

    static member inline (*) (v, k) =
      {
        X = v.X * k
        Y = v.Y * k
      }

    static member inline (*) (k, v) =
      {
        X = v.X * k
        Y = v.Y * k
      }

    static member inline (/) (v, k) =
      {
        X = v.X / k
        Y = v.Y / k
      }

type Vector2F = Vector2<float>

module Vector2 =

  let inline toTuple v = v.X, v.Y

  let inline fromAngle theta =
    {
      X = cos theta
      Y = sin theta
    }

  let inline lengthSquared v =
    v.X * v.X + v.Y * v.Y

  let inline length v =
    v.X * v.X + v.Y * v.Y |> sqrt

  let inline normalize v =
    let ls = lengthSquared v

    if ls = LanguagePrimitives.GenericZero
    then
      v
    else
      v / (sqrt ls)

module Vector2F =

  let zero = { X = 0.0; Y = 0.0 }
