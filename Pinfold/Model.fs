namespace Pinfold

open Thoth.Json.Net
open Thoth.Json.Giraffe

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

type Pinnery =
    { Name: string
      Location: string
      Pins: List<Pin> }

module Pinnery =
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
