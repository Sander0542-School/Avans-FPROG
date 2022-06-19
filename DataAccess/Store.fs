module Pinfold.Store

open Pinfold.Database

type Store() =
    member val pinneries: InMemoryDatabase<string, string * string> =
        [ "OggPins", "Lancre"
          "Pingum", "Genua"
          "LaBroche", "Quirm"
          "ASG", "Uberwald" ]
        |> Seq.map (fun t -> fst t, t)
        |> InMemoryDatabase.ofSeq

    member val pins: InMemoryDatabase<string, string * decimal * string> =
        [ "OP1", 100.0m, "OggPins"
          "OP2", 50m, "OggPins"
          "PG1", 10m, "Pingum"
          "LB Deluxe", 5m, "LaBroche"
          "SuperStifte", 13.37m, "ASG" ]
        |> Seq.map (fun (n, v, p) -> n, (n, v, p))
        |> InMemoryDatabase.ofSeq
        
    member val users: InMemoryDatabase<string, string * string * string> =
        [ "sander", "password", "" ]
        |> Seq.map (fun (username, password, favPinnery) -> username, (username, password, favPinnery))
        |> InMemoryDatabase.ofSeq
