package no.aileron.lispmachines

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class LispInterpreterTest extends FlatSpec with ShouldMatchers {

  val repl = new REPL()

  "Lisp code" should "add one with two" in {
    repl.executeLine("(+ 1 2)") should equal (3)
  }

  "Statements" should "be able to span multiple lines" in {
  repl.executeLine("""
    (+
      1
      2)
    """) should equal (3)
  }

  "Comments" should "be ignored" in {
    repl.executeLine("(+ 1 2) ; a comment") should equal (3)
    repl.execute("""
      ; a multiline comment
      ; it goes on
      ; and on
      (* 2 3) ; more Comments
      ; this shouldn't execute: (+ 10 20)
      """) should equal (List(6))
  }

  "An If" should "select the first branch if the condition is true" in {
    repl.executeLine("(if true 1 0)") should equal (1)
    repl.executeLine("(if (< 1 2) 1 0)") should equal (1)
  }

  it should "select the second branch if the condition is false" in {
    repl.executeLine("(if false 1 0)") should equal (0)
    repl.executeLine("(if (= 3 2) 1 0)") should equal (0)
  }

  "Functions" should "be quotable" in {
  repl.executeLine("'+") should equal ("+")
  repl.executeLine("(foldl '+ 0 '(1 2 3 4 5))") should equal (15)
}

"Multiple Functions with different arities" should "not shadow each other" in {
  repl.execute("""
    (defun test (a) 1)
    (defun test (a b) 2)
    (defun test (a b c) 3)
    (define test (lambda (a b c d) 4))
    """)

  evaluating { repl.executeLine("(test)") } should produce [MethodNotFound]
  repl.executeLine("(test 0)") should equal (1)
  repl.executeLine("(test 0 0)") should equal (2)
  repl.executeLine("(test 0 0 0)") should equal (3)
  repl.executeLine("(test 0 0 0 0)") should equal (4)
}

"Lambdas" should "be allowed" in {
  repl.executeLine("(define test (lambda (r) (* 3.141592653 (* r r))))")
}

it should "execute correctly" in {
  repl.executeLine("(define square (lambda (x) (* x x)))")
  repl.executeLine("(square 4)") should equal (16)
}
}
