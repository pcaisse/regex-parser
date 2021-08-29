import cats.parse.{Parser => P}
import cats.data.NonEmptyList

object Parser {

  enum SpecialChar(val char: Char):
    case LeftParen extends SpecialChar('(')
    case RightParen extends SpecialChar(')')
    case Period extends SpecialChar('.')
    case Pipe extends SpecialChar('|')
    case Star extends SpecialChar('*')

  private val parser = P.recursive[RegExp] { recurse =>
    val group =
      recurse
        .between(
          P.char(SpecialChar.LeftParen.char),
          P.char(SpecialChar.RightParen.char)
        )
        .map(Group(_))
    val nonSpecialChar =
      P.charWhere { char => !SpecialChar.values.map(_.char).contains(char) }
        .map(NonSpecialChar(_))
    val anyChar = P.char(SpecialChar.Period.char).map(_ => AnyChar())
    val zeroOrMore = (P.oneOf[Group | AnyChar | NonSpecialChar](
      group :: anyChar :: nonSpecialChar :: Nil
    ) <* P.char(SpecialChar.Star.char)).map(ZeroOrMore(_)).backtrack
    val individualRegExps =
      P.oneOf[ZeroOrMore | Group | AnyChar | NonSpecialChar](
        zeroOrMore :: group :: anyChar :: nonSpecialChar :: Nil
      )
    val sequence = individualRegExps.rep(2).map(Sequence(_)).backtrack
    val individualOrSequence =
      P.oneOf[Sequence | ZeroOrMore | Group | AnyChar | NonSpecialChar](
        sequence :: individualRegExps :: Nil
      )
    val or =
      ((individualOrSequence <* P.char(
        SpecialChar.Pipe.char
      )) ~ individualOrSequence)
        .map(Or(_, _))
        .backtrack
    P.oneOf(or :: sequence :: individualRegExps :: Nil)
  }

  val parse = parser.parse
}
