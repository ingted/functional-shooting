﻿namespace ShootingWebSharper

open System.Net
open WebSharper
open WebSharper.Html.Client
open WebSharper.JavaScript
open WebSharper.JQuery

module ShootingGame =

  type Mover = {
    x : float
    y : float
  }

  type Info = {
    playerShip : Mover
    bullets : Mover list
  }

  [<JavaScript>]
  let width = 400

  [<JavaScript>]
  let height = 600

  [<JavaScript>]
  let drawBackground (context : CanvasRenderingContext2D) =
    context.BeginPath()
    context.ClearRect(0., 0., float width, float height)
    context.Rect(0., 0., float width, float height)
    context.FillStyle <- "rgb(0, 0, 0)"
    context.Fill()

  module PlayerBullet =

    [<JavaScript>]
    let draw (context : CanvasRenderingContext2D) bullets =
      bullets
      |> List.iter(fun bullet ->
        context.Save()
        context.BeginPath()
        context.Translate(bullet.x, bullet.y)
        context.MoveTo(0., -4.)
        context.LineTo(0., 4.)
        context.StrokeStyle <- "rgb(255, 0, 0)"
        context.LineWidth <- 4.0
        context.LineCap <- LineCap.Round
        context.Stroke()
        context.Restore()
      )

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
  let initSocket context =

    let (|Text|Binary|) (data:obj) =
      match data with
      | :? string as text -> Text text
      | binary -> Binary (As<ArrayBuffer> binary)

    let draw info =
      info.playerShip |> PlayerShip.draw context
      info.bullets |> PlayerBullet.draw context

    let socket = WebSocket("ws://192.168.37.131:19860/shooting")
   
    socket.Onopen <- (fun () ->
      PlayerShip.init |> Json.Stringify |> socket.Send
    )
    
    socket.Onmessage <- (fun msg ->
      drawBackground context
      match msg.Data with
      | Text text -> text |> (Json.Parse >> As<Info> >> draw)
      | Binary _ -> ()
    )

    socket

  [<JavaScript>]
  let animatedCanvas width height =

    // キャンバスの設定
    let element = Tags.NewTag "Canvas" []
    let canvas  = As<CanvasElement> element.Dom
    canvas.Width  <- width
    canvas.Height <- height
    let context = canvas.GetContext "2d"
    
    // ソケットの準備
    let socket = initSocket context

    Div [ Width (string width); Attr.Style "float:left" ] -< [
      Div [ Attr.Style "float:center" ] -< [
        element
        |>! OnMouseMove (fun _ arg ->
          let offset = JQuery.JQuery.Of(element.Dom).Offset()
          (arg.X, arg.Y)
          ||> PlayerShip.nextPosition offset
          |> Json.Stringify
          |> socket.Send
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
  override __.Body = ShootingGame.Main () :> _
