package no.aileron.lispmachines

import scala.util.parsing.combinator._

case class Literal(l: String) { }
case class Replace(n: String)

object LispParser extends JavaTokenParsers {
  def parse(line: String) = parseAll(program, line) match {
    case Success(r, _) => r
    case _ => Nil
  }

  // grammar
  def program: Parser[List[Any]] = rep(exp)
  def list: Parser[List[Any]] = "("~>rep(exp)<~")"
  def exp: Parser[Any] = (
      real
    | hexInteger
    | integer
    | quote
    | literal
    | list
    | replacement
    | token
    )
  def integer: Parser[Long] = wholeNumber ^^ (n => n.toLong)
  def real: Parser[Double] = ( """\-?\d+\.\d*([eE]\-?\d+)?""".r ^^ (d => d.toDouble)
    | """\-?\d+[eE]\-?\d+""".r ^^ (d => d.toDouble) )
  def hexInteger: Parser[Long] = """\-?0x[\da-fA-F]+""".r ^^ (n => java.lang.Long.parseLong(n.substring(2), 16))
// TODO: added \n\r here .. should be in the REPL or code preprocessor or whatever -Magnus
  def token: Parser[String] = """[^() \n\r]+""".r ^^ (n => n.toString)
  def literal: Parser[Literal] = stringLiteral ^^ (l => Literal(l.tail.init))
  def quote = "'"~>exp ^^ (e => List("quote", e))
  def replacement = ","~>token ^^ (t => Replace(t))
}
