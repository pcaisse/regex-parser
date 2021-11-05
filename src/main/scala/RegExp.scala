import cats.data.NonEmptyList
import cats.Monoid
import cats.implicits._

abstract sealed class RegExp {
  def serialize: String
}

final case class NonSpecialChar(char: Char) extends RegExp {
  def serialize = this.char.toString
}
final case class AnyChar() extends RegExp {
  def serialize = RegExp.SpecialChar.Period.char.toString
}
final case class ZeroOrMore(regex: ZeroOrMoreRegExp) extends RegExp {
  def serialize = s"${regex.serialize}${RegExp.SpecialChar.Star.char}"
}
final case class Or(regexA: OrRegExp, regexB: OrRegExp) extends RegExp {
  def serialize =
    s"${regexA.serialize}${RegExp.SpecialChar.Pipe.char.toString}${regexB.serialize}"
}
final case class Sequence(
    regexes: NonEmptyList[SequenceRegExp]
) extends RegExp {
  def serialize = Monoid.combineAll(regexes.map(_.serialize).toList)
}
final case class Group(regex: RegExp) extends RegExp {
  def serialize =
    s"${RegExp.SpecialChar.LeftParen.char}${regex.serialize}${RegExp.SpecialChar.RightParen.char}"
}

type ZeroOrMoreRegExp = Group | AnyChar | NonSpecialChar
type SequenceRegExp = ZeroOrMore | Group | AnyChar | NonSpecialChar
type OrRegExp = Sequence | ZeroOrMore | Group | AnyChar | NonSpecialChar

object RegExp {
  enum SpecialChar(val char: Char):
    case LeftParen extends SpecialChar('(')
    case RightParen extends SpecialChar(')')
    case Period extends SpecialChar('.')
    case Pipe extends SpecialChar('|')
    case Star extends SpecialChar('*')
}
