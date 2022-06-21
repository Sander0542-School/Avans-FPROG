module Pinfold.Serialization

open Pinfold.Model
open Thoth.Json.Net

let encodePin: Encoder<Pin> =
    fun pin ->
        Encode.object [ "name", Encode.string pin.Name
                        "value", Encode.decimal pin.Value ]

let decodePin: Decoder<Pin> =
    Decode.object (fun get ->
        { Pin.Name = get.Required.Field "name" Decode.string
          Value = get.Required.Field "value" Decode.decimal })

let encodePinnery: Encoder<Pinnery> =
    fun pinnery ->
        let pins =
            pinnery.Pins |> List.map encodePin |> Encode.list

        Encode.object [ "name", Encode.string pinnery.Name
                        "location", Encode.string pinnery.Location
                        "pins", pins ]

let decodePinnery: Decoder<Pinnery> =
    Decode.object (fun get ->
        { Pinnery.Name = get.Required.Field "name" Decode.string
          Location = get.Required.Field "value" Decode.string
          Pins = get.Required.Field "pins" (Decode.list decodePin) })

let encodeUser: Encoder<User> =
    fun user ->
        let usernameEmail =
            match user.UsernameEmail with
            | Email email -> "email", Encode.string email
            | Username username -> "username", Encode.string username

        Encode.object [ "pinnery", Encode.option Encode.string user.Pinnery
                        usernameEmail ]

let decodeUserRaw (usernameEmailRaw: UsernameEmail) : Decoder<User> =
    let field, decoder =
        match usernameEmailRaw with
        | Email _ -> "email", (Decode.string |> Decode.map Email)
        | Username _ -> "username", (Decode.string |> Decode.map Username)

    Decode.object (fun get ->
        { UsernameEmail = get.Required.Field field decoder
          Password = get.Required.Field "password" Decode.string
          Pinnery = get.Optional.Field "pinnery" Decode.string })

let decodeUser: Decoder<User> =
    Decode.oneOf [ decodeUserRaw (Email "")
                   decodeUserRaw (Username "") ]
