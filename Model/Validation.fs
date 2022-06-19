module Pinfold.Validation

open System

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
            Seq.head errors

module Validator =
    let validateGreaterThan (than: decimal) (value: decimal) : Result<decimal, ValidationError> =
        if value > than then
            Ok value
        else
            Error(ValidationError $"{value} is not greater than {than}")

    let validateMinLength (length: int) (str: string) : Result<string, ValidationError> =
        match (validateGreaterThan length (String.length str)) with
        | Error _ -> Error(ValidationError $"Must be at least {length} characters")
        | Ok _ -> Ok str

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

    let validateNonEmpty = validateMinLength 0

module PinValidation =
    let validateNonEmptyName (pin: Pin) : Result<Pin, ValidationError> =
        match (Validator.validateNonEmpty pin.Name) with
        | Error _ -> Error(ValidationError "The name cannot be empty")
        | Ok _ -> Ok pin

    let validateUniqueName (pins: seq<Pin>) (pin: Pin) : Result<Pin, ValidationError> =
        let predicate =
            fun (pin1: Pin) -> pin1.Name <> pin.Name

        match (Validator.validateListExists predicate pins) with
        | Error _ -> Error(ValidationError "The name is not unique")
        | Ok _ -> Ok pin

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
            fun (pinnery1: Pinnery) -> pinnery.Name <> pinnery1.Name

        match (Validator.validateListExists predicate pinneries) with
        | Error _ -> Error(ValidationError "The name is not unique")
        | Ok _ -> Ok pinnery

    let validateLocation (pinnery: Pinnery) : Result<Pinnery, ValidationError> =
        match (Validator.validateListContains pinnery.Location validLocations) with
        | Error _ -> Error(ValidationError $"The {pinnery.Location} location is not valid")
        | Ok _ -> Ok pinnery

let validatePin (pins: seq<Pin>) (pin: Pin) : Result<Pin, ValidationError> =
    Validator.Multiple(
        pin,
        PinValidation.validateNonEmptyName,
        (PinValidation.validateUniqueName pins),
        PinValidation.validateValueAboveZero
    )

let validatePinnery (pinneries: seq<Pinnery>) (pinnery: Pinnery) : Result<Pinnery, ValidationError> =
    Validator.Multiple(
        pinnery,
        PinneryValidation.validateNonEmptyName,
        (PinneryValidation.validateUniqueName pinneries),
        PinneryValidation.validateLocation
    )
