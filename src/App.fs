module Asteroids.App

open Fable.Core
open Fable.Core.JS
open Browser
open Browser.Types
open System.Collections.Generic

[<Literal>]
let MaxSamples = 64

let mount (canvas : HTMLCanvasElement) (game : Game<_>) =

  let mutable state = game.Init ()

  let mutable keyboard = Keyboard.zero

  let mutable index = 0
  let mutable fpsSamples = new List<float> (Seq.init MaxSamples (fun _ -> 1000.0 / 60.0))

  let update () =
    state <- game.Update state

  // Connect inputs
  let keyHandler flag =
    (fun (e : KeyboardEvent) ->
      keyboard <-
        match e.keyCode with
        | 32.0 -> { keyboard with Space = flag }
        | 37.0 -> { keyboard with LeftArrow = flag }
        | 38.0 -> { keyboard with UpArrow = flag }
        | 39.0 -> { keyboard with RightArrow = flag }
        | 40.0 -> { keyboard with DownArrow = flag }
        | _ -> keyboard

      state <- game.ApplyInput keyboard state)

  canvas.onkeydown <- keyHandler true
  canvas.onkeyup <- keyHandler false

  // Update with a fixed timestep
  setInterval update (1000 / 60)
  |> ignore

  // Render as fast as possible
  let ctx = canvas.getContext_2d ()

  let rec renderLoop previousTime time =
    let deltaTime = time - previousTime

    fpsSamples.[index] <- deltaTime
    index <- (index + 1) % MaxSamples

    let fps = 1000.0 * float MaxSamples / (fpsSamples |> Seq.sum)

    // Clear canvas
    ctx.fillStyle <- U3.Case1 "magenta"
    ctx.fillRect (0.0, 0.0, canvas.width, canvas.height)

    // Game rendering
    ctx.save ()
    game.Render ctx state
    ctx.restore ()

    // FPS Counter
    ctx.save ()

    ctx.textAlign <- "left"
    ctx.textBaseline <- "top"

    let text = string (round fps)

    ctx.fillStyle <- U3.Case1 "black"
    ctx.fillText (text, 4.0, 4.0)
    ctx.fillStyle <- U3.Case1 "yellow"
    ctx.fillText (text, 5.0, 5.0)

    ctx.restore ()

    window.requestAnimationFrame (renderLoop time)
    |> ignore

  window.requestAnimationFrame (fun time -> renderLoop time time)
  |> ignore

  ()

let element =
  document.getElementById "root"
  :?> HTMLCanvasElement

mount element Asteroids.game
