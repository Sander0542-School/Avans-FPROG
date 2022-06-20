module Pinfold.Web

open Pinfold
open Pinfold.Database
open Pinfold.Store
open Giraffe
open Pinfold.Validation
open Thoth.Json.Net
open Thoth.Json.Giraffe


let getUsers: HttpHandler =
    fun next ctx ->
        task {
            let store = ctx.GetService<Store>()

            let users =
                InMemoryDatabase.all store.users
                |> Seq.map (fun (name, _, pinnery) ->
                    { Username = name
                      Password = ""
                      Pinnery = pinnery })

            return! ThothSerializer.RespondJsonSeq users Serialization.encodeUser next ctx
        }

let addUser: HttpHandler =
    fun next ctx ->
        task {
            let! decodedUser = ThothSerializer.ReadBody ctx Serialization.decodeUser

            match decodedUser with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok user ->
                let store = ctx.GetService<Store>()

                let pinneries =
                    InMemoryDatabase.all store.pinneries
                    |> Seq.map fst

                let otherUsers =
                    InMemoryDatabase.filter (fun (username, _, _) -> username = user.Username) store.users
                    |> Seq.map (fun (username, _, _) ->
                        { User.Username = username
                          Password = ""
                          Pinnery = None })

                match (validateUser otherUsers pinneries user) with
                | Error errors -> return! RequestErrors.BAD_REQUEST (errors |> Seq.toArray) next ctx
                | Ok validUser ->
                    InMemoryDatabase.insert
                        validUser.Username
                        (validUser.Username, User.hashPassword validUser.Password, validUser.Pinnery)
                        store.users
                    |> ignore

                    return! text "OK" next ctx
        }

let updateFavPinnery: HttpHandler =
    fun next ctx ->
        task {
            let! decodedUser = ThothSerializer.ReadBody ctx Serialization.decodeUser

            match decodedUser with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok user ->
                let store = ctx.GetService<Store>()

                let pinneries =
                    InMemoryDatabase.all store.pinneries
                    |> Seq.map fst

                let maybeUser =
                    InMemoryDatabase.filter
                        (fun (username, password, _) ->
                            username = user.Username
                            && password = User.hashPassword user.Password)
                        store.users
                    |> Seq.tryHead

                match maybeUser with
                | None -> return! RequestErrors.NOT_FOUND "User not found" next ctx
                | Some (username, password, _) ->
                    let pinneryValidation =
                        UserValidation.validatePinneryExists pinneries user

                    match pinneryValidation with
                    | Error (ValidationError error) -> return! RequestErrors.NOT_FOUND error next ctx
                    | Ok _ ->
                        InMemoryDatabase.update username (username, password, user.Pinnery) store.users
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

    let userRoutes =
        choose [ GET >=> getUsers
                 POST >=> addUser
                 PUT >=> updateFavPinnery ]

    choose [ subRoute "/user" userRoutes
             subRoutef "/%s" pinneryRoutes
             GET >=> getPinneries ]
