namespace Pinfold.Application.UserStore

open Pinfold.Model

type IUserStore =
    abstract all: seq<User>

    abstract get: UsernameEmail -> Option<User>

    abstract login: UsernameEmail -> string -> Option<User>

    abstract insert: User -> bool

    abstract update: User -> User -> unit
