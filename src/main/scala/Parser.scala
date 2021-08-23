import cats.parse.{Parser => P}

object Parser {
  val parser = P.recursive[RegExp] { recurse =>
    val group = recurse.between(P.char('('), P.char(')')).map(Group(_))
    val nonSpecialChar = P.anyChar.map(NonSpecialChar(_))
    val anyChar = P.char('.').map(_ => AnyChar())
    P.oneOf(group :: anyChar :: nonSpecialChar :: Nil)
  }
}
