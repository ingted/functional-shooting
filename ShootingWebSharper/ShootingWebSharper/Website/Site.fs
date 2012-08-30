namespace ShootingWebSharper

open System
open System.IO
open System.Web

open IntelliFactory.Html
open IntelliFactory.WebSharper.Sitelets
open IntelliFactory.WebSharper

type Games =
    | Index

module Site =
    let index =
      PageContent <| fun context ->
        let t = typeof<ShootingGameViewer>
        let element = Activator.CreateInstance(t) :?> Web.Control
        {
          Page.Default with
            Body = [Div [element]]
            Title = Some "Shooting WebSharper"
        }

    let controller =
        let handler = function
            | Index -> index

        { Handle = handler }

type Website() =
    interface IWebsite<Games> with
        member this.Actions = []
        member this.Sitelet =
            {
                Controller = Site.controller
                Router = Router.Table [Index, "/"] <|> Router.Infer()
            }

[<assembly: WebsiteAttribute(typeof<Website>)>]
do ()
