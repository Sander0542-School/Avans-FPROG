module Pinfold.DataAccess.UserStore

open Pinfold.Application.UserStore
open Pinfold.DataAccess.Database
open Pinfold.DataAccess.Store
open Pinfold.Model

type UserStore(store: Store) =
    let userStore = store.users

    let mapUser ((username, password, pinnery): string * string * Option<string>) : User =
        { User.Username = username
          Password = password
          Pinnery = pinnery }

    interface IUserStore with
        member this.all =
            InMemoryDatabase.all userStore |> Seq.map mapUser

        member this.get(username: string) =
            InMemoryDatabase.lookup username userStore
            |> Option.map mapUser

        member this.login (username: string) (password: string) =
            InMemoryDatabase.filter (fun (dbUsername, dbPassword, _) -> dbUsername = username && dbPassword = User.hashPassword password) userStore
            |> Seq.tryHead
            |> Option.map mapUser

        member this.insert(user: User) =
            let result =
                InMemoryDatabase.insert user.Username (user.Username, user.Password, user.Pinnery) userStore

            match result with
            | Ok _ -> true
            | Error _ -> false

        member this.update (user: User) (newUser: User) =
            InMemoryDatabase.update user.Username (newUser.Username, newUser.Password, newUser.Pinnery) userStore
