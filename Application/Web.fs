module Pinfold.Web

open Pinfold
open Pinfold.Database
open Pinfold.Store
open Giraffe
open Pinfold.Validation
open Thoth.Json.Net
open Thoth.Json.Giraffe


let getPinneries: HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let pinneries =
                InMemoryDatabase.all store.pinneries
                |> Seq.map (fun (name, loc) ->
                    // find pins for this pinnery
                    let pins =
                        InMemoryDatabase.filter (fun (_, _, p) -> p = name) store.pins
                        |> Seq.map (fun (name, value, _) -> { Pin.Name = name; Value = value })

                    { Name = name
                      Location = loc
                      Pins = List.ofSeq pins })

            return! ThothSerializer.RespondJsonSeq pinneries Serialization.encodePinnery next ctx
        }

let getPinnery (pinneryName: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let pinnery =
                InMemoryDatabase.lookup pinneryName store.pinneries
                |> Option.map (fun (_, loc) ->
                    let pins =
                        InMemoryDatabase.filter (fun (_, _, p) -> p = pinneryName) store.pins
                        |> Seq.map (fun (name, value, _) -> { Pin.Name = name; Value = value })

                    { Name = pinneryName
                      Location = loc
                      Pins = List.ofSeq pins })

            return!
                match pinnery with
                | None -> RequestErrors.NOT_FOUND "Pinnery not found!" next ctx
                | Some pinnery -> ThothSerializer.RespondJson pinnery Serialization.encodePinnery next ctx
        }

let addPinTo (pinneryName: string) : HttpHandler =
    fun next ctx ->
        task {
            let! decodedPin = ThothSerializer.ReadBody ctx Serialization.decodePin

            match decodedPin with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok pin ->
                let store = ctx.GetService<Store>()

                let otherPins =
                    InMemoryDatabase.filter (fun (_, _, p) -> p = pinneryName) store.pins
                    |> Seq.map (fun (name, value, _) -> { Pin.Name = name; Value = value })

                match (validatePin otherPins pin) with
                | Error errors ->
                    return! RequestErrors.BAD_REQUEST (errors |> Seq.toArray) next ctx
                | Ok validPin ->
                    InMemoryDatabase.insert validPin.Name (validPin.Name, validPin.Value, pinneryName) store.pins
                    |> ignore

                    return! text "OK" next ctx
        }

let pinsFor (pinneryName: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let pins =
                InMemoryDatabase.filter (fun (_, _, p) -> p = pinneryName) store.pins
                |> Seq.map (fun (name, value, _) -> { Pin.Name = name; Value = value })

            return! ThothSerializer.RespondJsonSeq pins Serialization.encodePin next ctx
        }


let valueForPin (pinneryName: string) (pinName: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            // First check if pin belongs to pinnery
            let maybePin =
                InMemoryDatabase.filter (fun (n, _, p) -> n = pinName && p = pinneryName) store.pins
                |> Seq.tryHead

            match maybePin with
            | None -> return! RequestErrors.NOT_FOUND "Pin not found" next ctx
            | Some (_, value, _) -> return! ThothSerializer.RespondJson value Encode.decimal next ctx
        }

let updateValueForPin (pinneryName: string) (pinName: string) : HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            // First check if pin belongs to pinnery
            let maybePin =
                InMemoryDatabase.filter (fun (n, _, p) -> n = pinName && p = pinneryName) store.pins
                |> Seq.tryHead

            match maybePin with
            | None -> return! RequestErrors.NOT_FOUND "Pin not found" next ctx
            | Some _ ->
                match! ThothSerializer.ReadBody ctx Decode.decimal with
                | Error e -> return! RequestErrors.BAD_REQUEST e next ctx
                | Ok newValue ->
                    InMemoryDatabase.update pinName (pinName, newValue, pinneryName) store.pins
                    return! text "OK!" next ctx
        }

let routes: HttpHandler =
    let pinneryRoutes pinneryName =
        choose [ GET
                 >=> subRoutef "/pin/%s/value" (valueForPin pinneryName)
                 PUT
                 >=> subRoutef "/pin/%s/value" (updateValueForPin pinneryName)
                 POST >=> subRoute "/pin" (addPinTo pinneryName)
                 GET >=> subRoute "/pin" (pinsFor pinneryName)
                 GET >=> getPinnery pinneryName ]

    choose [ subRoutef "/%s" pinneryRoutes
             GET >=> getPinneries ]
