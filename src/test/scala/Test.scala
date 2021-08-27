import org.junit.Test
import org.junit.Assert.*
import cats.data.NonEmptyList

def assertParsedEquals(input: String, expectedOutput: RegExp) =
  Parser.parse(input) match {
    case Right((_, actual)) =>
      assertEquals(expectedOutput, actual)
    case Left(errors) =>
      assert(false, s"Parsing failed with errors: ${errors.toString}")
  }

class ParserTest:
  @Test def nonSpecialChar(): Unit =
    assertParsedEquals("a", NonSpecialChar('a'))

  @Test def anyChar(): Unit =
    assertParsedEquals(".", AnyChar())

  @Test def group(): Unit =
    assertParsedEquals("(a)", Group(NonSpecialChar('a')))

  @Test def nestedGroup(): Unit =
    assertParsedEquals("((a))", Group(Group(NonSpecialChar('a'))))

  @Test def sequenceNonSpecial(): Unit =
    assertParsedEquals(
      "ab",
      Sequence(NonEmptyList.of(NonSpecialChar('a'), NonSpecialChar('b')))
    )

  @Test def sequenceNonSpecialAny(): Unit =
    assertParsedEquals(
      "a.",
      Sequence(NonEmptyList.of(NonSpecialChar('a'), AnyChar()))
    )

  @Test def sequenceWithGroup(): Unit =
    assertParsedEquals(
      "a(b)",
      Sequence(NonEmptyList.of(NonSpecialChar('a'), Group(NonSpecialChar('b'))))
    )

  @Test def or(): Unit =
    assertParsedEquals(
      "a|b",
      Or(NonSpecialChar('a'), NonSpecialChar('b'))
    )

  @Test def orGroups(): Unit =
    assertParsedEquals(
      "(a)|(b)",
      Or(Group(NonSpecialChar('a')), Group(NonSpecialChar('b')))
    )

  @Test def orGroupsOfSequenced(): Unit =
    assertParsedEquals(
      "(ab)|(cd)",
      Or(
        Group(Sequence(NonEmptyList.of(NonSpecialChar('a'), NonSpecialChar('b')))),
        Group(Sequence(NonEmptyList.of(NonSpecialChar('c'), NonSpecialChar('d'))))
      )
    )

  @Test def orSequences(): Unit =
    assertParsedEquals(
      "ab|cd",
      Or(
        Sequence(NonEmptyList.of(NonSpecialChar('a'), NonSpecialChar('b'))),
        Sequence(NonEmptyList.of(NonSpecialChar('c'), NonSpecialChar('d')))
      )
    )

  @Test def groupedOrSequences(): Unit =
    assertParsedEquals(
      "(ab|cd)",
      Group(Or(
        Sequence(NonEmptyList.of(NonSpecialChar('a'), NonSpecialChar('b'))),
        Sequence(NonEmptyList.of(NonSpecialChar('c'), NonSpecialChar('d')))
      ))
    )
