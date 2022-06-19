namespace Pinfold

type User =
    { Username: string
      Password: string
      FavoritePinnery: Pinnery }

module User =
    let hashPassword (_: string) = "secured!"
