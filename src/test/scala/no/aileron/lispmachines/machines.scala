package no.aileron.lispmachines

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class MachineSpec extends FlatSpec with ShouldMatchers {

	val w = new World()
	val m1 = new LispMachine("voyager", w)
	val m2 = new LispMachine("intrepid", w)
	val m3 = new ClockMachine("timekeeper", w)
	val m4 = new PrintMachine("timereader", w)
	val m5 = new LispMachine("unlinked", w)
	val l1 = w.newLink(m1, m2)
	val l2 = w.newLink(m2, m1)
	val l3 = w.newLink(m3, m4)

	val ls = w.links
	val ms = w.machines

	"Machines" should "be four" in {
		ms.size should equal (5)
	}

	it should "read time" in {
		w.tick
		// "timereader" should print the current second
	}

	it should "be able to overread input from link" in {
		m1.repl.executeLine("(input)") should equal (List()) // always a List
	}

	it should "be able to overread input from missing link" in {
		m5.repl.executeLine("(input)") should equal (List()) // always a List
	}

	"Links" should "be three" in {
		ls.size should equal (3)
	}

  "World" should "have working buffers" in {
		w.output(m1, List(5L))
		w.output(m1, List(10L))
		w.output(m2, List(3L))
		w.input(m2) should equal (List(5L))
		w.input(m1) should equal (List(3L))
		w.input(m2) should equal (List(10L))
	}

	it should "have buffers that work from lisp" in {
		m1.repl.executeLine("(output (list (+ 21 21) 72))")
		m2.repl.executeLine("(output (list 68 69))")
		m1.repl.executeLine("(output (list 43))")
		m2.repl.executeLine("(input)") should equal (List(21L + 21L, 72L))
		m2.repl.executeLine("(input)") should equal (List(43L))
		m1.repl.executeLine("(input)") should equal (List(68L, 69L))
	}

	it should "work with literals" in {
		m1.repl.executeLine("(output (list 'abc 2 'def))")
		m2.repl.executeLine("(input)") should equal (List("abc", 2, "def"))
	}

}
