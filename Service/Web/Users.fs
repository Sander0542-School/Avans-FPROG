module Pinfold.Web.Users

open Pinfold
open Pinfold.Model
open Pinfold.Application.PinneryStore
open Pinfold.Application.UserStore
open Giraffe
open Pinfold.Validation
open Thoth.Json.Giraffe

let pinneryName (pinnery: Pinnery) = pinnery.Name

let getUsers: HttpHandler =
    fun next ctx ->
        task {
            let userStore = ctx.GetService<IUserStore>()

            let users = userStore.all

            return! ThothSerializer.RespondJsonSeq users Serialization.encodeUser next ctx
        }

let addUser: HttpHandler =
    fun next ctx ->
        task {
            let! decodedUser = ThothSerializer.ReadBody ctx Serialization.decodeUser

            match decodedUser with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok user ->
                let pinneryStore =
                    ctx.GetService<IPinneryStore>()

                let pinneryNames =
                    pinneryStore.all |> Seq.map pinneryName

                let userStore = ctx.GetService<IUserStore>()
                let users = userStore.all

                match (validateUser users pinneryNames user) with
                | Error errors -> return! RequestErrors.BAD_REQUEST (errors |> Seq.toArray) next ctx
                | Ok validUser ->
                    match userStore.insert validUser with
                    | true -> return! text "OK" next ctx
                    | false -> return! ServerErrors.INTERNAL_ERROR "Failed" next ctx
        }

let updateFavPinnery: HttpHandler =
    fun next ctx ->
        task {
            let! decodedUser = ThothSerializer.ReadBody ctx Serialization.decodeUser

            match decodedUser with
            | Error errorMessage -> return! RequestErrors.BAD_REQUEST errorMessage next ctx
            | Ok updatedUser ->
                let pinneryStore =
                    ctx.GetService<IPinneryStore>()

                let pinneryNames =
                    pinneryStore.all |> Seq.map pinneryName

                let userStore = ctx.GetService<IUserStore>()

                let maybeUser =
                    userStore.login updatedUser.Username updatedUser.Password

                match maybeUser with
                | None -> return! RequestErrors.NOT_FOUND "User not found" next ctx
                | Some user ->
                    let pinneryValidation =
                        UserValidation.validatePinneryExists pinneryNames updatedUser

                    match pinneryValidation with
                    | Error (ValidationError error) -> return! RequestErrors.NOT_FOUND error next ctx
                    | Ok _ ->
                        let newUser =
                            { user with Pinnery = updatedUser.Pinnery }

                        userStore.update user newUser

                        return! text "OK" next ctx
        }
