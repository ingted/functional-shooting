namespace ShootingWebSharper

open System
open System.IO
open System.Web

open WebSharper
open WebSharper.Html.Server
open WebSharper.Sitelets

type Games =
    | Index

module Site =

  let index =
    PageContent <| fun _ ->
      {
        Page.Default with
          Body = [ Div [ new ShootingGameViewer() ] ]
          Title = Some "Shooting WebSharper"
      }

  let controller =
    let handle = function | Index -> index
    { Handle = handle }

type Website() =
  interface IWebsite<Games> with
    member __.Actions = []
    member __.Sitelet =
      {
        Controller = Site.controller
        Router = Router.Table [Index, "/"] <|> Router.Infer()
      }

[<assembly: WebsiteAttribute(typeof<Website>)>]
do ()
