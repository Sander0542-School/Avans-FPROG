namespace Pinfold

type User =
    { Username: string
      Password: string
      Pinnery: Option<string> }

module User =
    let hashPassword (_: string) = "secured!"
