import cats.data.NonEmptyList

final case class NonSpecialChar(char: Char)
final case class AnyChar()
final case class ZeroOrMore(regex: Group | AnyChar | NonSpecialChar)
final case class Or(regexA: OrRegExp, regexB: OrRegExp)
final case class Sequence(
    regexes: NonEmptyList[ZeroOrMore | Group | AnyChar | NonSpecialChar]
)
final case class Group(regex: RegExp)

type OrRegExp = Sequence | ZeroOrMore | Group | AnyChar | NonSpecialChar
type RegExp = NonSpecialChar | AnyChar | ZeroOrMore | Or | Sequence | Group
