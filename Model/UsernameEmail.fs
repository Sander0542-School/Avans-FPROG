namespace Pinfold.Model

open Microsoft.FSharp.Core

type UsernameEmail =
    | Email of string
    | Username of string

module UsernameEmailValidation =
    open Pinfold.Model.Validators

    let noSpaces =
        validateNotContains [ " " ] "The username cannot contains any spaces"

    let illegalWords =
        validateNotContains [ "nail"; "needle" ] "The username cannot contain needle or nail."

    let containsAt =
        validateContains [ "@" ] "The email must contain an @"

    let atNotBeginOrEnd (email: string) =
        if email.IndexOf "@" = 0
           || email.IndexOf "@" = email.Length - 1 then
            Error "The email must not start or end with an @"
        else
            Ok email

    let validateUsername (username: string) : Result<UsernameEmail, string> =
        username
        |> Ok
        |> Result.bind (nonEmpty "The username cannot be empty")
        |> Result.bind noSpaces
        |> Result.bind illegalWords
        |> Result.map Username

    let validateEmail (email: string) : Result<UsernameEmail, string> =
        email
        |> Ok
        |> Result.bind (minLength 3 "The The email must be at least 3 characters")
        |> Result.bind containsAt
        |> Result.bind atNotBeginOrEnd
        |> Result.map Email

    let validate
        (otherUsernameEmails: UsernameEmail list)
        (usernameEmail: UsernameEmail)
        : Result<UsernameEmail, string> =
        usernameEmail
        |> Ok
        |> Result.bind (validateIsUnique otherUsernameEmails "The username/email must be unique")
        |> Result.bind (fun (usernameEmail: UsernameEmail) ->
            match usernameEmail with
            | Username username -> validateUsername username
            | Email email -> validateUsername email)

module UsernameEmail =
    let make
        (otherUsernameEmails: UsernameEmail list)
        (option: string)
        (value: string)
        : Result<UsernameEmail, string> =
        match option with
        | "username" -> Ok(Username value)
        | "email" -> Ok(Email value)
        | _ -> Error "Invalid username/email option"
        |> Result.bind (UsernameEmailValidation.validate otherUsernameEmails)
