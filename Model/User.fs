namespace Pinfold.Model

open Microsoft.FSharp.Core
open Pinfold.Model

type User =
    { UsernameEmail: UsernameEmail
      Password: Password
      Pinnery: Option<string> }

module UserValidation =
    open Pinfold.Model.Validators

    let pinneryExists (pinneries: seq<Pinnery>) (maybePinnery: Option<string>) =
        match maybePinnery with
        | None -> Ok maybePinnery
        | Some pinneryName ->
            let predicate =
                fun (pinnery: Pinnery) -> pinnery.Name = pinneryName

            validateListExists pinneries "The pinnery does not exist" predicate
            |> Result.map (fun _ -> maybePinnery)

    let validatePinnery (pinneries: seq<Pinnery>) (pinnery: Option<string>) =
        pinnery
        |> Ok
        |> Result.bind (pinneryExists pinneries)

module User =
    let makeRaw (usernameEmail: UsernameEmail) (password: Password) (pinnery: Option<string>) =
        { User.UsernameEmail = usernameEmail
          Password = password
          Pinnery = pinnery }

    let make
        (users: seq<User>)
        (pinneries: seq<Pinnery>)
        (usernameEmailType: string)
        (usernameEmailValue: string)
        (passwordStr: string)
        (pinnery: Option<string>)
        : Result<User, string> =
        let otherUsernameEmails =
            users
            |> Seq.map (fun (user: User) -> user.UsernameEmail)
            |> Seq.toList

        let usernameEmailResult =
            UsernameEmail.make otherUsernameEmails usernameEmailType usernameEmailValue

        let passwordResult =
            Password.make passwordStr

        let pinneryResult =
            UserValidation.validatePinnery pinneries pinnery

        match usernameEmailResult with
        | Error value -> Error value
        | Ok usernameEmail ->
            match passwordResult with
            | Error value -> Error value
            | Ok password ->
                match pinneryResult with
                | Error value -> Error value
                | Ok pinnery ->
                    Ok
                        { User.UsernameEmail = usernameEmail
                          Password = password
                          Pinnery = pinnery }
