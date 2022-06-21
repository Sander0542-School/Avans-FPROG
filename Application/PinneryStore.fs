namespace Pinfold.Application.PinneryStore

open Pinfold.Model

type IPinneryStore =
    abstract all: seq<Pinnery>

    abstract get: string -> Option<Pinnery>
