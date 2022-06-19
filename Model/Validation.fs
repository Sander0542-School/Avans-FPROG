module Pinfold.Validation

open System
open Microsoft.FSharp.Core

type ValidationError = ValidationError of string

type Validator =
    static member Multiple(item: 'a, [<ParamArray>] validators: ('a -> Result<'a, ValidationError>) array) =
        let errors =
            validators
            |> Seq.map (fun validator -> validator item)
            |> Seq.filter (fun result ->
                match result with
                | Error _ -> true
                | Ok _ -> false)

        if Seq.isEmpty errors then
            Ok item
        else
            errors
            |> Seq.map (fun result ->
                match result with
                | Error (ValidationError message) -> message
                | _ -> String.Empty)
            |> Seq.filter (fun str -> (not (String.IsNullOrWhiteSpace str)))
            |> Error

module Validator =
    let validateGreaterThan (than: decimal) (value: decimal) : Result<decimal, ValidationError> =
        if value > than then
            Ok value
        else
            Error(ValidationError $"{value} is not greater than {than}")

    let validateMinLength (length: int) (str: string) : Result<string, ValidationError> =
        if String.length str < length then
            Error(ValidationError $"Must be at least {length} characters")
        else
            Ok str

    let validateListContains (item: 'a) (list: seq<'a>) : Result<'a, ValidationError> =
        if Seq.contains item list then
            Ok item
        else
            Error(ValidationError $"The list does not contain {item}")

    let validateListExists (predicate: 'a -> bool) (list: seq<'a>) : Result<seq<'a>, ValidationError> =
        if Seq.exists predicate list then
            Ok list
        else
            Error(ValidationError "The list does not contain the given item")

    let validateContainsSubstring (str: string) (subStr: string) : Result<string, ValidationError> =
        if (str.Contains(subStr)) then
            Ok str
        else
            Error(ValidationError $"{str} does not contain {subStr}")

    let validateNonEmpty = validateMinLength 1

module PinValidation =
    let validateNonEmptyName (pin: Pin) : Result<Pin, ValidationError> =
        match (Validator.validateNonEmpty pin.Name) with
        | Error _ -> Error(ValidationError "The name cannot be empty")
        | Ok _ -> Ok pin

    let validateUniqueName (pins: seq<Pin>) (pin: Pin) : Result<Pin, ValidationError> =
        let predicate =
            fun (pin1: Pin) -> pin1.Name = pin.Name

        match (Validator.validateListExists predicate pins) with
        | Error _ -> Ok pin
        | Ok _ -> Error(ValidationError "The name is not unique")

    let validateValueAboveZero (pin: Pin) : Result<Pin, ValidationError> =
        match (Validator.validateGreaterThan 0 pin.Value) with
        | Error _ -> Error(ValidationError "The value needs to be bigger than zero")
        | Ok _ -> Ok pin

module PinneryValidation =
    let validLocations =
        [ "Ankh-Morpork"
          "Quirm"
          "Genua"
          "Omnia"
          "Lancre"
          "Uberwald" ]

    let validateNonEmptyName (pinnery: Pinnery) : Result<Pinnery, ValidationError> =
        match (Validator.validateNonEmpty pinnery.Name) with
        | Error _ -> Error(ValidationError "The name cannot be empty")
        | Ok _ -> Ok pinnery

    let validateUniqueName (pinneries: seq<Pinnery>) (pinnery: Pinnery) : Result<Pinnery, ValidationError> =
        let predicate =
            fun (pinnery1: Pinnery) -> pinnery.Name = pinnery1.Name

        match (Validator.validateListExists predicate pinneries) with
        | Error _ -> Ok pinnery
        | Ok _ -> Error(ValidationError "The name is not unique")

    let validateLocation (pinnery: Pinnery) : Result<Pinnery, ValidationError> =
        match (Validator.validateListContains pinnery.Location validLocations) with
        | Error _ -> Error(ValidationError $"The {pinnery.Location} location is not valid")
        | Ok _ -> Ok pinnery

module UserValidation =
    let illegalWords = [ "nail"; "needle" ]

    let validateNoIllegalWords
        (func: User -> string)
        (errorMessage: string)
        (user: User)
        : Result<User, ValidationError> =
        let testStr = func user

        let predicate =
            fun (str: string) -> testStr.Contains(str)

        match (Validator.validateListExists predicate illegalWords) with
        | Error _ -> Ok user
        | Ok _ -> Error(ValidationError errorMessage)

    let validateUsernameNotEmpty (user: User) : Result<User, ValidationError> =
        match (Validator.validateNonEmpty user.Username) with
        | Error _ -> Error(ValidationError "The name cannot be empty")
        | Ok _ -> Ok user

    let validateUsernameNoSpaces (user: User) : Result<User, ValidationError> =
        match (Validator.validateContainsSubstring user.Username " ") with
        | Error _ -> Ok user
        | Ok _ -> Error(ValidationError "The username cannot contains any spaces")

    let validateUsernameNoIllegalWords (user: User) =
        validateNoIllegalWords (fun (user: User) -> user.Username) "The username cannot contain illegal words" user

    let validateUsernameUnique (users: seq<User>) (user: User) : Result<User, ValidationError> =
        let predicate =
            fun (user1: User) -> user.Username = user1.Username

        match (Validator.validateListExists predicate users) with
        | Error _ -> Ok user
        | Ok _ -> Error(ValidationError "The username is already used")

    let validatePasswordNoIllegalWords (user: User) =
        validateNoIllegalWords (fun (user: User) -> user.Password) "The password cannot contain illegal words" user

    let validatePasswordLength (user: User) : Result<User, ValidationError> =
        match (Validator.validateMinLength 10 user.Password) with
        | Ok _ -> Ok user
        | Error _ -> Error(ValidationError "The password needs to be at least 10 characters long")

    let validatePasswordHasDigit (user: User) : Result<User, ValidationError> =
        if String.exists Char.IsDigit user.Password then
            Ok user
        else
            Error(ValidationError "The password needs to contains a digit")

    let validatePasswordHasUppercaseLetter (user: User) : Result<User, ValidationError> =
        if String.exists Char.IsUpper user.Password then
            Ok user
        else
            Error(ValidationError "The password needs to contains an uppercase letter")

let validatePin (pins: seq<Pin>) (pin: Pin) : Result<Pin, seq<string>> =
    Validator.Multiple(
        pin,
        PinValidation.validateNonEmptyName,
        (PinValidation.validateUniqueName pins),
        PinValidation.validateValueAboveZero
    )

let validatePinnery (pinneries: seq<Pinnery>) (pinnery: Pinnery) : Result<Pinnery, seq<string>> =
    Validator.Multiple(
        pinnery,
        PinneryValidation.validateNonEmptyName,
        (PinneryValidation.validateUniqueName pinneries),
        PinneryValidation.validateLocation
    )

let validateUser (users: seq<User>) (user: User) : Result<User, seq<string>> =
    Validator.Multiple(
        user,
        UserValidation.validateUsernameNotEmpty,
        UserValidation.validateUsernameNoSpaces,
        UserValidation.validateUsernameNoIllegalWords,
        (UserValidation.validateUsernameUnique users),
        UserValidation.validatePasswordNoIllegalWords,
        UserValidation.validatePasswordLength,
        UserValidation.validatePasswordHasDigit,
        UserValidation.validatePasswordHasUppercaseLetter
    )
