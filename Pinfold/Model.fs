namespace Pinfold

open System
open Pinfold
open Thoth.Json.Net
open Thoth.Json.Giraffe

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

type Pin = { Name: string; Value: decimal }

module Pin =
    let encode: Encoder<Pin> =
        fun pin ->
            Encode.object [ "name", Encode.string pin.Name
                            "value", Encode.decimal pin.Value ]

    let decode: Decoder<Pin> =
        Decode.object (fun get ->
            { Name = get.Required.Field "name" Decode.string
              Value = get.Required.Field "value" Decode.decimal })

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

    let validate (pins: seq<Pin>) (pin: Pin) : Result<Pin, ValidationError> =
        Validator.Multiple(pin, validateNonEmptyName, (validateUniqueName pins), validateValueAboveZero)

type Pinnery =
    { Name: string
      Location: string
      Pins: List<Pin> }

module Pinnery =
    let validLocations =
        [ "Ankh-Morpork"
          "Quirm"
          "Genua"
          "Omnia"
          "Lancre"
          "Uberwald" ]

    let encode: Encoder<Pinnery> =
        fun pinnery ->
            let pins =
                pinnery.Pins |> List.map Pin.encode |> Encode.list

            Encode.object [ "name", Encode.string pinnery.Name
                            "location", Encode.string pinnery.Location
                            "pins", pins ]

    let decode: Decoder<Pinnery> =
        Decode.object (fun get ->
            { Name = get.Required.Field "name" Decode.string
              Location = get.Required.Field "value" Decode.string
              Pins = get.Required.Field "pins" (Decode.list Pin.decode) })

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

    let validate (pinneries: seq<Pinnery>) (pinnery: Pinnery) : Result<Pinnery, ValidationError> =
        Validator.Multiple(pinnery, validateNonEmptyName, (validateUniqueName pinneries), validateLocation)
