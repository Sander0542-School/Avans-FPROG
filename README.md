# Introduction

This repository contains a working web service application for Pinfold. Your task is to refactor this application using the functional programming principles discussed during the course. Furthermore, you will implement additional use cases.

# Model

The application currently models pins and pinneries

## Pinnery

A pinnery produces pins. Each pinnery

1. has a *unique *name that is not empty,
2. and has a location. Valid locations are the fictive places of
   - Ankh-Morpork
   - Quirm
   - Genua
   - Omnia
   - Lancre
   - Uberwald

## Pinfold

A pin

1. has a unique name that is not empty,
2. a value in dollars that cannot be below zero,
3. and was produced by a known pinnery

# Code discussion

## `Program.md`

1. The application starts in the `main` function.
2. The function `configureApp` registers the routes defined in `Web.fs`.
3. The function `configureServices` registers the data store and JSON-library Thoth that are used by the HttpHandlers in `Web.fs`.

## `Database.md`

Defines a simple in-memory database that stores objects associated with a primary key. The objects are of type `'T`. Keys are of type `'Key`.

```fsharp
type InMemoryDatabase<'Key, 'T when 'Key: comparison>
```

## `Store.fs`

Defines separate in-memory databases to store pinneries and pins.

A Pinnery is stored as a tuple:

1. first element is the name
2. the second element is the pinnery's location.

A pin is stored as a tuple

1. first element is the name of the pin
2. second element is the value as a decimal
3. third element is the name of an existing pinnery

## `Model.fs`

Defines structures for Pins and Pinneries

## `Web.fs`

Contains the http handlers
