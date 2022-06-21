namespace Pinfold.Model

type UsernameEmail =
    | Email of string
    | Username of string

type User =
    { UsernameEmail: UsernameEmail
      Password: string
      Pinnery: Option<string> }

module User =
    let hashPassword (_: string) = "secured!"
