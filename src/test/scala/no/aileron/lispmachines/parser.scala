package no.aileron.lispmachines

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class LispParserTest extends FlatSpec with ShouldMatchers {

  val lp = LispParser

  "Lisp code" should "be tokenized" in {
    lp.parse("(+ 1 2)") should equal (List(List("+", 1, 2)))
  }

  "Lisp statements" should "be able to span multiple lines" in {
    lp.parse("""
      (+
        1
        2)
      """) should equal (List(List("+", 1, 2)))
  }
}
