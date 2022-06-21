module Pinfold.DataAccess.PinStore

open Pinfold.Application.PinStore
open Pinfold.DataAccess.Database
open Pinfold.DataAccess.Store
open Pinfold.Model

type PinStore(store: Store) =
    let pinStore = store.pins

    let mapPin ((name, value, _): string * decimal * string) : Pin = { Pin.Name = name; Value = value }

    interface IPinStore with
        member this.all =
            InMemoryDatabase.all pinStore |> Seq.map mapPin

        member this.wherePinnery(pinneryName: string) =
            InMemoryDatabase.filter (fun (_, _, p) -> p = pinneryName) pinStore
            |> Seq.map mapPin

        member this.get(name: string) =
            InMemoryDatabase.lookup name pinStore
            |> Option.map mapPin

        member this.getWherePinnery (pinneryName: string) (pinName: string) =
            InMemoryDatabase.filter (fun (n, _, p) -> n = pinName && p = pinneryName) pinStore
            |> Seq.tryHead
            |> Option.map mapPin

        member this.insert (pinnery: Pinnery) (pin: Pin) =
            let result =
                InMemoryDatabase.insert pin.Name (pin.Name, pin.Value, pinnery.Name) pinStore

            match result with
            | Ok _ -> true
            | Error _ -> false

        member this.update (pinnery: Pinnery) (pin: Pin) (newPin: Pin) =
            InMemoryDatabase.update pin.Name (newPin.Name, newPin.Value, pinnery.Name) pinStore
