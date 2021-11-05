import cats.parse.{Parser => P}
import cats.data.NonEmptyList

object RegexParser {

  private enum SpecialChar(val char: Char):
    case LeftParen extends SpecialChar('(')
    case RightParen extends SpecialChar(')')
    case Period extends SpecialChar('.')
    case Pipe extends SpecialChar('|')
    case Star extends SpecialChar('*')

  /*
   * Regex group (regex surrounded by parentheses).
   */
  private def group(recurse: P[RegExp]): P[Group] =
    recurse
      .between(
        P.char(SpecialChar.LeftParen.char),
        P.char(SpecialChar.RightParen.char)
      )
      .map(Group(_))

  /*
   * Any character that isn't one of the special ones.
   */
  private val nonSpecialChar: P[NonSpecialChar] =
    P.charWhere { char => !SpecialChar.values.map(_.char).contains(char) }
      .map(NonSpecialChar(_))

  /*
   * Period character (which means any).
   */
  private val anyChar: P[AnyChar] =
    P.char(SpecialChar.Period.char).map(_ => AnyChar())

  val parser = P.recursive[RegExp] { recurse =>
    val individualRegExps = (P.oneOf[Group | AnyChar | NonSpecialChar](
      group(recurse) :: anyChar :: nonSpecialChar :: Nil
    ) ~ P.char(SpecialChar.Star.char).?)
      .map[ZeroOrMore | Group | AnyChar | NonSpecialChar] {
        case (regex, Some(_)) =>
          // Found a star, so wrap in zero-or-more
          ZeroOrMore(regex)
        case (regex, None) => regex
      }
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
}
