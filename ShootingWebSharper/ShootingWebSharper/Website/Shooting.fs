namespace ShootingWebSharper

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Html5

module ShootingGame =

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
  let animatedCanvas width height =

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
        do! Async.Sleep (1000 / fps)
        do! loop
      }
    Async.Start loop

    Div [ Width (string width); Attr.Style "float:left" ] -< [
      Div [ Attr.Style "float:center" ] -< [
        element
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
