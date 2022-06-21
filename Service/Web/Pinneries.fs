module Pinfold.Web.Pinneries

open Pinfold
open Pinfold.Application.PinneryStore
open Pinfold.Application.PinStore
open Giraffe
open Pinfold.Validation
open Thoth.Json.Net
open Thoth.Json.Giraffe

let getPinneries: HttpHandler =
    fun next ctx ->
        task {
            let pinneryStore =
                ctx.GetService<IPinneryStore>()

            let pinneries = pinneryStore.all

            return! ThothSerializer.RespondJsonSeq pinneries Serialization.encodePinnery next ctx
        }

let getPinnery (pinneryName: string) : HttpHandler =
    fun next ctx ->
        task {
            let pinneryStore =
                ctx.GetService<IPinneryStore>()

            let pinnery = pinneryStore.get pinneryName

            match pinnery with
            | None -> return! RequestErrors.NOT_FOUND "Pinnery not found!" next ctx
            | Some pinnery -> return! ThothSerializer.RespondJson pinnery Serialization.encodePinnery next ctx
        }

let addPinTo (pinneryName: string) : HttpHandler =
    fun next ctx ->
        task {
            let! decodedPin = ThothSerializer.ReadBody ctx Serialization.decodePin

            match decodedPin with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok pin ->
                let pinneryStore =
                    ctx.GetService<IPinneryStore>()

                let maybePinnery =
                    pinneryStore.get pinneryName

                match maybePinnery with
                | None -> return! RequestErrors.NOT_FOUND "Pinnery not found!" next ctx
                | Some pinnery ->
                    let pinStore = ctx.GetService<IPinStore>()

                    let otherPins = pinStore.all

                    match (validatePin otherPins pin) with
                    | Error errors -> return! RequestErrors.BAD_REQUEST (errors |> Seq.toArray) next ctx
                    | Ok validPin ->
                        match pinStore.insert pinnery validPin with
                        | true -> return! text "OK" next ctx
                        | false -> return! ServerErrors.INTERNAL_ERROR "Failed" next ctx
        }

let pinsFor (pinneryName: string) : HttpHandler =
    fun next ctx ->
        task {
            let pinneryStore =
                ctx.GetService<IPinneryStore>()

            let maybePinnery =
                pinneryStore.get pinneryName

            match maybePinnery with
            | None -> return! RequestErrors.NOT_FOUND "Pinnery not found!" next ctx
            | Some pinnery -> return! ThothSerializer.RespondJsonSeq pinnery.Pins Serialization.encodePin next ctx
        }


let valueForPin (pinneryName: string) (pinName: string) : HttpHandler =
    fun next ctx ->
        task {
            let pinneryStore =
                ctx.GetService<IPinneryStore>()

            let maybePinnery =
                pinneryStore.get pinneryName

            match maybePinnery with
            | None -> return! RequestErrors.NOT_FOUND "Pinnery not found" next ctx
            | Some pinnery ->
                let pinStore = ctx.GetService<IPinStore>()

                let maybePin =
                    pinStore.getWherePinnery pinnery.Name pinName

                match maybePin with
                | None -> return! RequestErrors.NOT_FOUND "Pin not found" next ctx
                | Some pin -> return! ThothSerializer.RespondJson pin.Value Encode.decimal next ctx
        }

let updateValueForPin (pinneryName: string) (pinName: string) : HttpHandler =
    fun next ctx ->
        task {
            let pinneryStore =
                ctx.GetService<IPinneryStore>()

            let maybePinnery =
                pinneryStore.get pinneryName

            match maybePinnery with
            | None -> return! RequestErrors.NOT_FOUND "Pinnery not found" next ctx
            | Some pinnery ->
                let pinStore = ctx.GetService<IPinStore>()

                let maybePin =
                    pinStore.getWherePinnery pinnery.Name pinName

                match maybePin with
                | None -> return! RequestErrors.NOT_FOUND "Pin not found" next ctx
                | Some pin ->
                    match! ThothSerializer.ReadBody ctx Decode.decimal with
                    | Error e -> return! RequestErrors.BAD_REQUEST e next ctx
                    | Ok newValue ->
                        let newPin = { pin with Value = newValue }
                        pinStore.update pinnery pin newPin

                        return! text "OK" next ctx
        }
