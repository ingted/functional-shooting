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

  [<JavaScript>]
  let drawPlayerShip (context : CanvasRenderingContext2D) playerShip =
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
  let movePlayerShip (offset : Position) _ (point : Events.MouseEvent) =
    let x = float (point.X - offset.Left)
    let y = float (point.Y - offset.Top)
    { x = x; y = y }

  [<JavaScript>]
  let rec internal gameLoop context playerShip =
    async {
      do drawBackground context
      do drawPlayerShip context !playerShip
      do! Async.Sleep (1000 / fps)
      do! gameLoop context playerShip
    }

  [<JavaScript>]
  let animatedCanvas width height =

    let playerShip = ref { x = (float width) / 2.; y = (float height) / 3. * 2. }

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
        |>! OnMouseMove (fun e arg ->
          let offset = JQuery.JQuery.Of(element.Dom).Offset()
          playerShip := movePlayerShip offset e arg
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
