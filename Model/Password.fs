namespace Pinfold.Model

open System

type Password = private Password of string

module PasswordValidation =
    open Pinfold.Model.Validators
    
    let noIllegalWords =
        validateNotContains [ "nail"; "needle" ] "The password cannot contain needle or nail."

    let passwordLength =
        minLength 10 "The password must be at least 10 characters long"

    let containsDigit =
        validateStringExists Char.IsDigit "The password needs to contains a digit"

    let containsUppercaseLetter =
        validateStringExists Char.IsUpper "The password needs to contains an uppercase letter"

    let validate (password: string) =
        password
        |> Ok
        |> Result.bind passwordLength
        |> Result.bind noIllegalWords
        |> Result.bind containsDigit
        |> Result.bind containsUppercaseLetter

module Password =
    let hashPassword (_: string) = Password "secured!"

    let make (rawPassword: string) =
        rawPassword
        |> PasswordValidation.validate
        |> Result.map hashPassword

    let fromHashed (hashedPassword: string) = hashedPassword |> Password
