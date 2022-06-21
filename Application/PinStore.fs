namespace Pinfold.Application.PinStore

open Pinfold.Model

type IPinStore =
    abstract all: seq<Pin>
    
    abstract wherePinnery: string -> seq<Pin>

    abstract get: string -> Option<Pin>
    
    abstract getWherePinnery: string -> string -> Option<Pin>

    abstract insert: Pinnery -> Pin -> bool

    abstract update: Pinnery -> Pin -> Pin -> unit
