namespace Pinfold.Store

open Pinfold.Database

type IStore =
    abstract pinneries: InMemoryDatabase<string, string * string>

    abstract pins: InMemoryDatabase<string, string * decimal * string>

    abstract users: InMemoryDatabase<string, string * string * Option<string>>
