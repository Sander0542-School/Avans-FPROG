module Pinfold.HttpHandlers

open Giraffe
open Pinfold.Web.Users
open Pinfold.Web.Pinneries

let routes: HttpHandler =
    let pinneryRoutes pinneryName =
        choose [ GET
                 >=> subRoutef "/pin/%s/value" (valueForPin pinneryName)
                 PUT
                 >=> subRoutef "/pin/%s/value" (updateValueForPin pinneryName)
                 POST >=> subRoute "/pin" (addPinTo pinneryName)
                 GET >=> subRoute "/pin" (pinsFor pinneryName)
                 GET >=> getPinnery pinneryName ]

    let userRoutes =
        choose [ GET >=> getUsers
                 POST >=> addUser
                 PUT >=> updateFavPinnery ]

    choose [ subRoute "/user" userRoutes
             subRoutef "/%s" pinneryRoutes
             GET >=> getPinneries ]
