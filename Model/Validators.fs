module Pinfold.Model.Validators

let minLength (length: int) (errorMessage: string) (str: string) : Result<string, string> =
    if String.length str < length then
        Error errorMessage
    else
        Ok str

let nonEmpty = minLength 1

let validateListExists (list: seq<'a>) (errorMessage: string) (predicate: 'a -> bool) : Result<seq<'a>, string> =
    if Seq.exists predicate list then
        Ok list
    else
        Error errorMessage

let validateContains (items: string list) (errorMessage: string) (str: string) : Result<string, string> =
    let predicate =
        fun (item: string) -> item.Contains(str)

    validateListExists items errorMessage predicate
    |> Result.map (fun _ -> str)

let validateNotContains (items: string list) (errorMessage: string) (str: string) : Result<string, string> =
    let predicate =
        fun (item: string) -> (not (item.Contains(str)))

    validateListExists items errorMessage predicate
    |> Result.map (fun _ -> str)

let validateIsUnique (items: 'a list) (errorMessage: string) (item: 'a) =
    let predicate =
        fun (item1: 'a) -> (not (item1 = item))

    validateListExists items errorMessage predicate
    |> Result.map (fun _ -> item)

let validateStringExists (predicate: char -> bool) (errorMessage: string) (str: string) : Result<string, string> =
    if String.exists predicate str then
        Ok str
    else
        Error errorMessage
