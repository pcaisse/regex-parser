import org.junit.Test
import cats.data.NonEmptyList

class RegExpTest:
  @Test def serializeNonSpecialChar(): Unit =
    assert(NonSpecialChar('a').serialize == "a")

  @Test def serializeAnyChar(): Unit =
    assert(AnyChar().serialize == ".")

  @Test def serializeZeroOrMore(): Unit =
    assert(ZeroOrMore(NonSpecialChar('a')).serialize == "a*")

  @Test def serializeOr(): Unit =
    assert(Or(NonSpecialChar('a'), NonSpecialChar('b')).serialize == "a|b")

  @Test def serializeGroup(): Unit =
    assert(Group(NonSpecialChar('a')).serialize == "(a)")

  @Test def serializeSequence(): Unit =
    assert(
      Sequence(
        NonEmptyList.of(NonSpecialChar('a'), NonSpecialChar('b'))
      ).serialize == "ab"
    )
