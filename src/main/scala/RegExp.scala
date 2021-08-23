import cats.data.NonEmptyList

final case class NonSpecialChar(char: Char)
final case class AnyChar()
final case class ZeroOrMore(regex: RegExp)
final case class Or(regexA: RegExp, regexB: RegExp)
final case class Sequence(regexes: NonEmptyList[RegExp])
final case class Group(regex: RegExp)

type RegExp = NonSpecialChar | AnyChar | ZeroOrMore | Or | Sequence | Group
