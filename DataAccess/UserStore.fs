module Pinfold.DataAccess.UserStore

open Pinfold.Application.UserStore
open Pinfold.DataAccess.Database
open Pinfold.DataAccess.Store
open Pinfold.Model

type UserStore(store: Store) =
    let userStore = store.users

    let mapUser ((usernameEmail, password, pinnery): UsernameEmail * string * Option<string>) : User =
        { User.UsernameEmail = usernameEmail
          Password = password
          Pinnery = pinnery }

    interface IUserStore with
        member this.all =
            InMemoryDatabase.all userStore |> Seq.map mapUser

        member this.get(usernameEmail: UsernameEmail) =
            InMemoryDatabase.lookup usernameEmail userStore
            |> Option.map mapUser

        member this.login (usernameEmail: UsernameEmail) (password: string) =
            InMemoryDatabase.filter (fun (dbUsername, dbPassword, _) -> dbUsername = usernameEmail && dbPassword = User.hashPassword password) userStore
            |> Seq.tryHead
            |> Option.map mapUser

        member this.insert(user: User) =
            let result =
                InMemoryDatabase.insert user.UsernameEmail (user.UsernameEmail, user.Password, user.Pinnery) userStore

            match result with
            | Ok _ -> true
            | Error _ -> false

        member this.update (user: User) (newUser: User) =
            InMemoryDatabase.update user.UsernameEmail (newUser.UsernameEmail, newUser.Password, newUser.Pinnery) userStore
