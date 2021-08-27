import cats.parse.{Parser => P}
import cats.parse.Rfc5234.{alpha, digit}
import cats.data.NonEmptyList

object Parser {

  val leftParen = P.char('(')
  val rightParen = P.char(')')
  val period = P.char('.')
  val pipe = P.char('|')

  private val parser = P.recursive[RegExp] { recurse =>
    val group =
      recurse.between(leftParen, rightParen).map(Group(_))
    val nonSpecialChar = (alpha | digit).map(NonSpecialChar(_))
    val anyChar = period.map(_ => AnyChar())
    val individualRegExps =
      P.oneOf[Group | AnyChar | NonSpecialChar](
        group :: anyChar :: nonSpecialChar :: Nil
      )
    val sequence = individualRegExps.rep(2).map(Sequence(_)).backtrack
    val or =
      ((individualRegExps <* pipe) ~ individualRegExps).map(Or(_, _)).backtrack
    P.oneOf(sequence :: or :: individualRegExps :: Nil)
  }

  val parse = parser.parse
}
