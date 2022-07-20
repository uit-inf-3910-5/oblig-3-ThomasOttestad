module Server

open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection


open FSharp.Control.Tasks.V2
open Thoth.Json.Net
open Saturn
open Giraffe
open Shared


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"

let port = 
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let dataFile = 
    __SOURCE_DIRECTORY__ + "/data.json"


let handleGetModel next (ctx : HttpContext) = 
    let data = System.IO.File.ReadAllText dataFile
    let decoder = Decode.Auto.generateDecoder<Todo> ()
    task {
        match Decode.fromString decoder data with
        | Ok temp -> return! json temp next ctx
        | Error err -> return! RequestErrors.BAD_REQUEST (text err) next ctx
    }

let handleAddModel next (ctx : HttpContext) =
    let txt = System.IO.File.ReadAllText dataFile
    let decoder = Decode.Auto.generateDecoder<Todo> ()
    let people = 
        match Decode.fromString decoder txt with
        | Ok p -> p
        | Error _ -> Todo.New
    task {
        try 
            let! data = ctx.BindJsonAsync<Todo> ()
            let p = Encode.Auto.toString (4, (data))
            System.IO.File.WriteAllText(dataFile, p)
            return! json data next ctx
        with exn ->
            return! RequestErrors.BAD_REQUEST (text exn.Message) next ctx
    }



let handleResetServer next (ctx : HttpContext) = 
    System.IO.File.WriteAllText(dataFile, "")

    let data = System.IO.File.ReadAllText dataFile
    let decoder = Decode.Auto.generateDecoder<Todo> ()
    task {
        match Decode.fromString decoder data with
        | Ok temp -> return! json temp next ctx
        | Error err -> return! RequestErrors.BAD_REQUEST (text err) next ctx
    }

let webApp = 
    choose [
        GET >=> route "/api/model" >=> handleGetModel
        POST >=> route "/api/model" >=> handleAddModel
        DELETE >=> route "/api/reset" >=> handleResetServer
    ]

let jsonSerializer = Thoth.Json.Giraffe.ThothSerializer ()

let configureServices (services : IServiceCollection) = 
    services.AddGiraffe() |> ignore
    services.AddSingleton<Serialization.Json.IJsonSerializer>(jsonSerializer) |> ignore

let configureApp (app : IApplicationBuilder) = 
    app.UseDefaultFiles()
        .UseStaticFiles()
        .UseGiraffe webApp

WebHost 
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()