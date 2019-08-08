open System.Threading.Tasks

open Giraffe
open Saturn
open LegacyShared

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Atrous

//let publicPath = Path.GetFullPath "../LegacyClient/public"

let getInitCounter() : Task<string list> = task { return ["Hello"]}

let bartenderApi = {
    print = fun dbPath paths printerName -> 
        async {
            return Bartender.printWithXlsx dbPath paths printerName
        } 
}

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue bartenderApi
    |> Remoting.buildHttpHandler

let app = application {
    url (Route.hostName + "/")
    use_router webApp
    memory_cache
    //use_static publicPath
    use_gzip
}

run app
