module Pinfold.DataAccess.PinneryStore

open Pinfold.Application.PinneryStore
open Pinfold.Application.PinStore
open Pinfold.DataAccess.Database
open Pinfold.DataAccess.Store
open Pinfold.Model

type PinneryStore(store: Store, pinStore: IPinStore) =
    let pinneryStore = store.pinneries

    let mapPinnery ((name, location): string * string) : Pinnery =
        let pins =
            pinStore.wherePinnery name |> Seq.toList

        { Pinnery.Name = name
          Location = location
          Pins = pins }

    interface IPinneryStore with
        member this.all =
            InMemoryDatabase.all pinneryStore
            |> Seq.map mapPinnery

        member this.get(name: string) =
            InMemoryDatabase.lookup name pinneryStore
            |> Option.map mapPinnery
