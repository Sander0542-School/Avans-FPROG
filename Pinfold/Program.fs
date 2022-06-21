open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Pinfold.DataAccess.Store
open Pinfold.DataAccess.UserStore
open Pinfold.DataAccess.PinneryStore
open Pinfold.DataAccess.PinStore
open Pinfold.Application.UserStore
open Pinfold.Application.PinneryStore
open Pinfold.Application.PinStore
open Thoth.Json.Giraffe
open Thoth.Json.Net
open Pinfold

let configureApp (app: IApplicationBuilder) =
    // Add Giraffe to the ASP.NET Core pipeline
    app.UseGiraffe HttpHandlers.routes

let configureServices (services: IServiceCollection) =
    // Add Giraffe dependencies
    services
        .AddGiraffe()
        .AddSingleton<Store>(Store())
        .AddSingleton<IUserStore, UserStore>()
        .AddSingleton<IPinneryStore, PinneryStore>()
        .AddSingleton<IPinStore, PinStore>()
        .AddSingleton<Json.ISerializer>(ThothSerializer(skipNullField = false, caseStrategy = CaseStrategy.CamelCase))
    |> ignore

[<EntryPoint>]
let main _ =
    Host
        .CreateDefaultBuilder()
        .ConfigureWebHostDefaults(fun webHostBuilder ->
            webHostBuilder
                .Configure(configureApp)
                .ConfigureServices(configureServices)
            |> ignore)
        .Build()
        .Run()

    0
