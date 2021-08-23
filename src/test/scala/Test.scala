import org.junit.Test
import org.junit.Assert.*

def assertParsedEquals(input: String, expectedOutput: RegExp) =
  Parser.parser.parse(input) match {
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
