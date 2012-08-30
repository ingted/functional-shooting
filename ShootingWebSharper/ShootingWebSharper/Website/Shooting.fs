namespace ShootingWebSharper

open System.Net
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5
open IntelliFactory.WebSharper.JQuery

module ShootingGame =

  type PlayerShip = {
    x : float
    y : float
  }

  [<JavaScript>]
  let width = 300

  [<JavaScript>]
  let height = 300

  [<JavaScript>]
  let fps = 60

  [<JavaScript>]
  let drawBackground (ctx : CanvasRenderingContext2D) =
    ctx.BeginPath()
    ctx.Rect(0., 0., float width, float height)
    ctx.FillStyle <- "rgb(0, 0, 0)"
    ctx.Fill()

  [<JavaScript>]
  let drawPlayerShip (ctx : CanvasRenderingContext2D) playerShip =
    ctx.Save()
    ctx.BeginPath()
    ctx.Translate(playerShip.x, playerShip.y)
    ctx.MoveTo(0., -10.)
    ctx.LineTo(-10., 10.)
    ctx.LineTo(10., 10.)
    ctx.FillStyle <- "rgb(64, 64, 255)"
    ctx.Fill()
    ctx.Restore()

  [<JavaScript>]
  let movePlayerShip (element : Element) playerShip _ (point : Events.MouseEvent) =
    let offset = JQuery.JQuery.Of(element.Dom).Offset()
    let x = float (point.X - offset.Left)
    let y = float (point.Y - offset.Top)
    playerShip := { x = x; y = y }

  [<JavaScript>]
  let animatedCanvas width height =

    let playerShip = ref { x = 0.; y = 0. }

    // キャンバスの設定
    let element = Tags.NewTag "Canvas" []
    let canvas  = As<CanvasElement> element.Dom
    canvas.Width  <- width
    canvas.Height <- height
    let ctx = canvas.GetContext "2d"

    // ゲームループ
    let rec loop =
      async {
        do drawBackground ctx
        do drawPlayerShip ctx !playerShip
        do! Async.Sleep (1000 / fps)
        do! loop
      }
    Async.Start loop

    Div [ Width (string width); Attr.Style "float:left" ] -< [
      Div [ Attr.Style "float:center" ] -< [
        element
        |>! OnMouseMove (movePlayerShip element playerShip)
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
