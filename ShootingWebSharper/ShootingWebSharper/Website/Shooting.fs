namespace ShootingWebSharper

open System.Net
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5
open IntelliFactory.WebSharper.JQuery

module ShootingGame =

  type Mover = {
    x : float
    y : float
  }

  [<JavaScript>]
  let width = 400

  [<JavaScript>]
  let height = 600

  [<JavaScript>]
  let fps = 60

  [<JavaScript>]
  let drawBackground (context : CanvasRenderingContext2D) =
    context.BeginPath()
    context.Rect(0., 0., float width, float height)
    context.FillStyle <- "rgb(0, 0, 0)"
    context.Fill()

  module PlayerShip =

    [<JavaScript>]
    let init = { x = (float width) / 2.; y = (float height) / 3. * 2. }

    [<JavaScript>]
    let nextPosition (offset : Position) x y =
      let nextX = float (x - offset.Left)
      let nextY = float (y - offset.Top)
      { x = nextX; y = nextY }

    [<JavaScript>]
    let draw (context : CanvasRenderingContext2D) playerShip =
      context.Save()
      context.BeginPath()
      context.Translate(playerShip.x, playerShip.y)
      context.MoveTo(0., -10.)
      context.LineTo(-10., 10.)
      context.LineTo(10., 10.)
      context.FillStyle <- "rgb(64, 64, 255)"
      context.Fill()
      context.Restore()

    [<JavaScript>]
    let move offset x y playerShip =
      playerShip := nextPosition offset x y

  [<JavaScript>]
  let rec internal gameLoop context playerShip =
    async {
      do drawBackground context
      do !playerShip |> PlayerShip.draw context
      do! Async.Sleep (1000 / fps)
      do! gameLoop context playerShip
    }

  [<JavaScript>]
  let animatedCanvas width height =

    let playerShip = ref PlayerShip.init

    // キャンバスの設定
    let element = Tags.NewTag "Canvas" []
    let canvas  = As<CanvasElement> element.Dom
    canvas.Width  <- width
    canvas.Height <- height
    let context = canvas.GetContext "2d"

    // ゲームループ開始
    Async.Start (gameLoop context playerShip)

    Div [ Width (string width); Attr.Style "float:left" ] -< [
      Div [ Attr.Style "float:center" ] -< [
        element
        |>! OnMouseMove (fun _ arg ->
          let offset = JQuery.JQuery.Of(element.Dom).Offset()
          playerShip |> PlayerShip.move offset arg.X arg.Y
        )
      ]
    ]

  [<JavaScript>]
  let Main () =
    Div [
      animatedCanvas width height
      Div [Attr.Style "clear:both"]
    ]

type ShootingGameViewer() =
  inherit Web.Control()
  [<JavaScript>]
  override this.Body = ShootingGame.Main () :> _
