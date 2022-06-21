module Pinfold.DataAccess.Store

open Pinfold.Model
open Pinfold.DataAccess.Database

type Store() =
    member val pinneries: InMemoryDatabase<string, string * string> =
        [ "OggPins", "Lancre"
          "Pingum", "Genua"
          "LaBroche", "Quirm"
          "ASG", "Uberwald" ]
        |> Seq.map (fun (name, location) -> name, (name, location))
        |> InMemoryDatabase.ofSeq

    member val pins: InMemoryDatabase<string, string * decimal * string> =
        [ "OP1", 100.0m, "OggPins"
          "OP2", 50m, "OggPins"
          "PG1", 10m, "Pingum"
          "LB Deluxe", 5m, "LaBroche"
          "SuperStifte", 13.37m, "ASG" ]
        |> Seq.map (fun (name, value, pinnery) -> name, (name, value, pinnery))
        |> InMemoryDatabase.ofSeq

    member val users: InMemoryDatabase<UsernameEmail, UsernameEmail * string * Option<string>> =
        [ Email "sander@mail.com", User.hashPassword "Password12", None
          Username "jeroen", User.hashPassword "Password34", Some "Pingum"
          Username "martijn", User.hashPassword "Password56", Some "LaBroche" ]
        |> Seq.map (fun (usernameEmail, password, favPinnery) -> usernameEmail, (usernameEmail, password, favPinnery))
        |> InMemoryDatabase.ofSeq
