namespace Pinfold.Application.UserStore

open Pinfold.Model

type IUserStore =
    abstract all: seq<User>

    abstract get: string -> Option<User>

    abstract login: string -> string -> Option<User>

    abstract insert: User -> bool

    abstract update: User -> User -> unit
