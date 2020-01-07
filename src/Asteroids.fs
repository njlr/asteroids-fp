namespace Asteroids

type Ship =
  {
    IsAlive : bool
    IsThrusting : bool
    Turning : bool option
    Orientation : float
    Position : Vector2F
    Velocity : Vector2F
    Cooldown : float
  }

type Bullet =
  {
    Position : Vector2F
    Velocity : Vector2F
  }

type Asteroid =
  {
    Position : Vector2F
    Velocity : Vector2F
    Polygon : Vector2F list
    HasAppeared : bool
  }

module Utils =

  let wrapPosition (p : Vector2F) : Vector2F =
    let rec fixUp m x =
      if x < 0.0
      then
        fixUp m (x + m)
      else if x > m
      then
        x % m
      else
        x
    {
      X = p.X |> fixUp 640.0
      Y = p.Y |> fixUp 480.0
    }

  open Browser.Types
  open Fable.Core.JsInterop

  let setImageSmoothing (enabled : bool) (ctx : CanvasRenderingContext2D) =
    ctx?("imageSmoothingEnabled") <- enabled

module Asteroid =

  open System

  let private r = Random ()

  let private random m =
    r.NextDouble () |> float |> (*) m

  let private randomCircle () =
    Vector2.fromAngle (random <| 2.0 * Math.PI)

  let init () =
    let nPoints = 5
    let center = { X = 320.0; Y = 240.0 }
    let position = center + (randomCircle ()) * (360.0 + random 160.0)
    {
      Position = position
      Velocity = Vector2.normalize ((center + randomCircle () * 256.0) - position) * (0.2 + random 1.0)
      Polygon =
        [0..nPoints]
        |> Seq.map (fun x -> Vector2.fromAngle (2.0 * Math.PI * float x / float nPoints) * (12.0 + random 16.0))
        |> Seq.toList
      HasAppeared = false
    }

  let update (asteroid : Asteroid) =
    {
      asteroid with
        HasAppeared =
          asteroid.HasAppeared ||
          (
            asteroid.Position.X > 0.0 &&
            asteroid.Position.Y > 0.0 &&
            asteroid.Position.X < 640.0 &&
            asteroid.Position.Y < 480.0
          )
        Position =
          asteroid.Position + asteroid.Velocity
          |> (if asteroid.HasAppeared then Utils.wrapPosition else id)
    }

type Simulation =
  {
    Ship : Ship
    Bullets : Bullet list
    Asteroids : Asteroid list
    AsteroidTimer : float
    Score : int
  }

module Simulation =

  let updateShip state =
    let nextVelocity =
      if state.Ship.IsThrusting
      then
        state.Ship.Velocity + (Vector2.fromAngle state.Ship.Orientation) * 0.2
      else
        state.Ship.Velocity * 0.98

    {
      state with
        Ship =
          {
            state.Ship with
              Position =
                state.Ship.Position + nextVelocity
                |> Utils.wrapPosition
              Velocity = nextVelocity
              Orientation =
                state.Ship.Orientation +
                (
                  state.Ship.Turning
                  |> Option.map (fun x -> if x then -0.08 else 0.08)
                  |> Option.defaultValue 0.0
                )
              Cooldown =
                max 0.0 (state.Ship.Cooldown - 1.0)
          }
        Bullets =
          state.Bullets
          |> Seq.map (fun b -> { b with Position = b.Position + b.Velocity })
          |> Seq.filter (fun b ->
            b.Position.X > -32.0 &&
            b.Position.Y > -32.0 &&
            b.Position.X < 640.0 + 32.0 &&
            b.Position.Y < 480.0 + 32.0)
          |> Seq.toList
    }

  let applyInput keyboard state =
    let state =
      {
        state with
          Ship =
            {
              state.Ship with
                IsThrusting = keyboard.UpArrow
                Turning =
                  match keyboard.LeftArrow, keyboard.RightArrow with
                  | (true, false) -> Some true
                  | (false, true) -> Some false
                  | _ -> None
            }
      }

    if (keyboard.Space || keyboard.DownArrow) && state.Ship.IsAlive && state.Ship.Cooldown = 0.0
    then
      let v = Vector2.fromAngle state.Ship.Orientation
      let bullet =
        {
          Position = state.Ship.Position + v * 16.0
          Velocity = v * 4.0
        }

      {
        state with
          Ship =
            {
              state.Ship with
                Cooldown = 40.0
            }
          Bullets = bullet :: state.Bullets
      }
    else
      state

  let collideBullets state =
    let intersections =
      state.Bullets
      |> Seq.allPairs state.Asteroids
      |> Seq.filter (fun (asteroid, bullet) ->
        let ds = Vector2.lengthSquared (asteroid.Position - bullet.Position)
        ds < 24.0 * 24.0
      )
      |> Seq.toList

    {
      state with
        Score = state.Score + Seq.length intersections
        Asteroids = state.Asteroids |> List.except (intersections |> Seq.map fst)
        Bullets = state.Bullets |> List.except (intersections |> Seq.map snd)
    }

  let collideShip state =
    if state.Ship.IsAlive
    then
      {
        state with
          Ship =
            {
              state.Ship with
                IsAlive =
                  state.Asteroids
                  |> Seq.forall (fun asteroid ->
                    let ds = Vector2.lengthSquared (asteroid.Position - state.Ship.Position)
                    ds > 24.0 * 24.0
                  )
            }
      }
    else
      state

  let update state =
    let state = updateShip state

    let state =
      if state.AsteroidTimer <= 0.0 && Seq.length state.Asteroids < 32
      then
        {
          state with
            AsteroidTimer = 600.0
            Asteroids = List.init 6 (fun _ -> Asteroid.init ()) @ state.Asteroids
        }
      else
        {
          state with
            AsteroidTimer = state.AsteroidTimer - 1.0
        }

    let state =
      {
        state with
          Asteroids =
            state.Asteroids
            |> List.map Asteroid.update
      }

    let state = collideBullets state

    let state = collideShip state

    state

  open Browser.Types
  open Fable.Core
  open System

  let render (ctx : CanvasRenderingContext2D) state =
    ctx.save ()

    Utils.setImageSmoothing false ctx

    ctx.fillStyle <- U3.Case1 "black"
    ctx.fillRect (0.0, 0.0, ctx.canvas.width, ctx.canvas.height)

    ctx.strokeStyle <- U3.Case1 "white"
    ctx.strokeRect (0.0, 0.0, ctx.canvas.width, ctx.canvas.height)

    ctx.restore ()

    // Bullets
    ctx.save ()
    ctx.strokeStyle <- U3.Case1 "white"

    for bullet in state.Bullets do
      ctx.save ()

      ctx.translate (Vector2.toTuple bullet.Position)
      ctx.beginPath()
      ctx.arc(0.0, 0.0, 6.0, 0.0, 2.0 * Math.PI)
      ctx.closePath ()
      ctx.stroke()

      ctx.restore ()

    ctx.restore ()

    // Asteroids
    ctx.save ()
    ctx.strokeStyle <- U3.Case1 "white"

    for asteroid in state.Asteroids do
      ctx.save ()

      ctx.translate (Vector2.toTuple asteroid.Position)
      ctx.beginPath()

      for p in asteroid.Polygon do
        ctx.lineTo (Vector2.toTuple p)

      ctx.closePath ()
      ctx.stroke()

      ctx.restore ()

    ctx.restore ()

    // Ship
    if state.Ship.IsAlive
    then
      ctx.save ()

      ctx.strokeStyle <- U3.Case1 "white"
      ctx.translate (Vector2.toTuple state.Ship.Position)
      ctx.rotate state.Ship.Orientation
      ctx.beginPath ()
      ctx.moveTo (16.0, 0.0)
      ctx.lineTo (-12.0, -12.0)
      ctx.lineTo (-12.0, 12.0)
      ctx.closePath ()
      ctx.stroke ()

      ctx.restore ()

type Screen =
| Start
| Playing of Simulation
| GameOver of Simulation * float

module Playing =

  let init () =
    Playing
      {
        Score = 0
        Ship =
          {
            IsAlive = true
            IsThrusting = false
            Turning = None
            Orientation = 0.0
            Position = { X = 320.0; Y = 240.0 }
            Velocity = Vector2F.zero
            Cooldown = 0.0
          }
        Bullets = []
        Asteroids = []
        AsteroidTimer = 60.0
      }

  let applyInput keyboard state =
    Simulation.applyInput keyboard state |> Playing

  let update state =
    let nextState = Simulation.update state

    if nextState.Ship.IsAlive
    then
      Playing nextState
    else
      GameOver (nextState, 180.0)

module Screen =

  open Fable.Core
  open Browser.Types

  let applyInput keyboard state =
    match state with
    | Start ->
      if keyboard.Space
      then
        Playing.init ()
      else
        state
    | Playing state ->
      Playing.applyInput keyboard state
    | GameOver (_, cooldown) ->
      if cooldown <= 0.0 && keyboard.Space
      then
        Start
      else
        state

  let update state =
    match state with
    | Start ->
      state
    | Playing state ->
      Playing.update state
    | GameOver (state, cooldown) ->
      GameOver (Simulation.update state, max (cooldown - 1.0) 0.0)

  let render (ctx : CanvasRenderingContext2D) state =
    match state with
    | Start ->
      ctx.save ()

      Utils.setImageSmoothing false ctx

      ctx.fillStyle <- U3.Case1 "black"
      ctx.fillRect (0.0, 0.0, ctx.canvas.width, ctx.canvas.height)

      ctx.font <- "48px sans-serif"
      ctx.textAlign <- "center"
      ctx.textBaseline <- "middle"
      ctx.fillStyle <- U3.Case1 "white"
      ctx.fillText ("ASTEROIDS", 320.0, 240.0)

      ctx.font <- "16px sans-serif"
      ctx.fillText ("PRESS SPACE TO PLAY", 320.0, 240.0 + 32.0)

      ctx.strokeStyle <- U3.Case1 "white"
      ctx.strokeRect (0.0, 0.0, ctx.canvas.width, ctx.canvas.height)

      ctx.restore ()

    | Playing state ->
      Simulation.render ctx state

      ctx.save ()
      ctx.font <- "16px sans-serif"
      ctx.textAlign <- "center"
      ctx.textBaseline <- "middle"
      ctx.fillStyle <- U3.Case1 "white"
      ctx.fillText (sprintf "SCORE %i" state.Score, 320.0, 480.0 - 32.0)
      ctx.restore ()

    | GameOver (state, cooldown) ->
      Simulation.render ctx state

      ctx.save ()

      ctx.font <- "48px sans-serif"
      ctx.textAlign <- "center"
      ctx.textBaseline <- "middle"
      ctx.fillStyle <- U3.Case1 "white"
      ctx.fillText ("GAME OVER", 320.0, 240.0)

      if cooldown <= 0.0
      then
        ctx.font <- "16px sans-serif"
        ctx.fillText ("PRESS SPACE", 320.0, 240.0 + 32.0)

      ctx.restore ()

module Asteroids =

  let game =
    {
      Init = fun () -> Start
      ApplyInput = Screen.applyInput
      Update = Screen.update
      Render = Screen.render
    }
