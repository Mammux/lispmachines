package no.aileron.lispmachines

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class IOSpec extends FlatSpec with ShouldMatchers {
	val repl = new REPL()

	"Input" should "be 14" in {
		repl.executeLine("(input)") should equal (List(14L))
	}

	it should "be 28 when doubled" in {
		repl.executeLine("(+ (car (input)) (car (input)))") should equal (28L)
	}

	"Output" should "work for longs" in {
		repl.executeLine("(output (list 42)")
	}

	it should "work for literals" in {
		repl.executeLine("(output (list 'abc)")
	}


}
