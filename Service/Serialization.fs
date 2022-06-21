module Pinfold.Serialization

open Pinfold.Model
open Thoth.Json.Net

let encodePin: Encoder<Pin> =
    fun pin ->
        Encode.object [ "name", Encode.string pin.Name
                        "value", Encode.decimal pin.Value ]

let decodePin: Decoder<Pin> =
    Decode.object (fun get ->
        { Name = get.Required.Field "name" Decode.string
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
        { Name = get.Required.Field "name" Decode.string
          Location = get.Required.Field "value" Decode.string
          Pins = get.Required.Field "pins" (Decode.list decodePin) })

let encodeUser: Encoder<User> =
    fun user ->
        Encode.object ["username", Encode.string user.Username
                       "pinnery", Encode.option Encode.string user.Pinnery ]

let decodeUser: Decoder<User> =
    Decode.object (fun get ->
        { Username = get.Required.Field "username" Decode.string
          Password = get.Required.Field "password" Decode.string
          Pinnery = get.Optional.Field "pinnery" Decode.string })