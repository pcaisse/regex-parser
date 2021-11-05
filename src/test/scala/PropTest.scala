import cats.data.NonEmptyList

import munit.ScalaCheckSuite
import org.scalacheck._
import org.scalacheck.Prop._
import Gen._
import Arbitrary.arbitrary

object Generators {

  val genAnyChar: Gen[AnyChar] = const(AnyChar())

  val genNonSpecialChar: Gen[NonSpecialChar] = for {
    c <- arbitrary[Char]
  } yield NonSpecialChar(c)

  val genZeroOrMore: Gen[ZeroOrMoreRegExp] =
    Gen.frequency(
      5 -> genNonSpecialChar,
      3 -> genAnyChar,
      1 -> Gen.lzy(genGroup)
    )

  val genSequence: Gen[Sequence] = {
    val genSequenceRegExp: Gen[SequenceRegExp] =
      Gen.frequency(
        5 -> genNonSpecialChar,
        3 -> genAnyChar,
        1 -> Gen.lzy(genGroup),
        1 -> Gen.lzy(genZeroOrMore)
      )
    for {
      first <- genSequenceRegExp
      rest <- Gen.nonEmptyListOf(genSequenceRegExp)
    } yield {
      Sequence(NonEmptyList(first, rest))
    }
  }

  val genOr: Gen[Or] = {
    val genOrRegExp: Gen[OrRegExp] =
      Gen.frequency(
        3 -> genNonSpecialChar,
        3 -> genAnyChar,
        1 -> Gen.lzy(genZeroOrMore),
        1 -> Gen.lzy(genGroup),
        1 -> Gen.lzy(genSequence)
      )
    for {
      a <- genOrRegExp
      b <- genOrRegExp
    } yield Or(a, b)
  }

  val genRegExp: Gen[RegExp] = Gen.frequency(
    3 -> genNonSpecialChar,
    2 -> genAnyChar,
    1 -> Gen.lzy(genSequence),
    1 -> Gen.lzy(genZeroOrMore),
    1 -> Gen.lzy(genOr),
    1 -> Gen.lzy(genGroup)
  )

  val genGroup: Gen[Group] = for {
    regex <- genRegExp
  } yield Group(regex)
}

class PropTest extends ScalaCheckSuite {

  property("regex") {
    forAll(Generators.genRegExp) { (regex: RegExp) =>
      RegexParser.parser.parse(regex.serialize) match {
        case Right((_, actual)) =>
          actual == regex
        case _ =>
          false
      }
    }
  }

}
