package no.aileron.lispmachines

class MySeq[T](l: Seq[T]) {
  def toDouble = try {
      l.map {
        case l: Long => l.toDouble
        case d: Double => d
      }
    } catch {
      case e: MatchError => throw new TypeError("couldn't convert to double: " + e)
    }

  def toLong = try {
      l.map {
        case l: Long => l
        case d: Double => d.toLong
      }
    } catch {
      case e: MatchError => throw new TypeError("couldn't convert to long: " + e)
    }

  def allLong = l.forall {
    case l: Long => true
    case _ => false
  }

  def eval(env: Env) = l.map(e => Interpreter.eval(e, env))
}

object Helper {
  implicit def Seq2MySeq[T](l: Seq[T]) = new MySeq[T](l)
}

import Helper._

object Builtins {
  def compare(
    op: (Double, Double) => Boolean,
    initOffset: Double,
    n: Seq[Any],
    env: Env
    ): Boolean = {
    val xs = n.eval(env).toDouble
    xs.fold( (xs(0) + initOffset, true) ) {
      case ( (prev: Double, valid: Boolean), cur: Double) =>
        if(valid && op(prev, cur)) (cur, true) else (cur, false)
    } match { case (_, flag: Boolean) => flag }
  }

  def opD(l: List[Any], f: (Double, Double) => Double, env: Env) = {
    l.eval(env).toDouble.reduce(f)
  }

  def opL(l: List[Any], f: (Long, Long) => Long, env: Env) = {
    l.eval(env).toLong.reduce(f)
  }

  def argCount(l: List[Any], n: Int) {
    if(l.length - 1 != n) throw new InterpreterException(
      "invalid number of arguments: expected %d, got %d".format(n, l.length - 1))
  }

  val ops = List("+", "-", "*", "/", "%", "min", "max")

  def builtins(l: List[Any], env: Env): PartialFunction[String, Any] = {
    // arithmetic
    case op: String if ops.contains(op) =>
      val args = l.tail.eval(env)
      op match {
        case "+" => if(args.allLong) args.toLong.reduce(_ + _) else args.toDouble.reduce(_ + _)
        case "-" => if(args.allLong) args.toLong.reduce(_ - _) else args.toDouble.reduce(_ - _)
        case "*" => if(args.allLong) args.toLong.reduce(_ * _) else args.toDouble.reduce(_ * _)
        case "/" => if(args.allLong) args.toLong.reduce(_ / _) else args.toDouble.reduce(_ / _)
        case "min" => if(args.allLong) args.toLong.min else args.toDouble.min
        case "max" => if(args.allLong) args.toLong.max else args.toDouble.max
      }

    // comparisons
    case "<" => compare(_ < _, -1, l.tail, env)
    case ">" => compare(_ > _, 1, l.tail, env)
    case ">=" => compare(_ >= _, 0, l.tail, env)
    case "<=" => compare(_ <= _, 0, l.tail, env)
    case "=" => l.tail.eval(env).distinct.length == 1
    case "!=" => l.tail.eval(env).distinct.length > 1

    case "atom" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case l: List[Any] => false
      case _ => true
    }

    // string furnctions
    case "to-string" => argCount(l, 1); Interpreter.eval(l(1), env).toString
    case "concat" => l.tail.eval(env).mkString

    // list functions
    case "car" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case l: List[Any] => l.head
      case s: String => s.head
      case Literal(s) => s.head
      case _ => throw new TypeError("can't get head of non-list")
    }

    case "cdr" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case l: List[Any] => l.tail
      case s: String => s.tail
      case Literal(s) => s.tail
      case _ => throw new TypeError("can't get tail of non-list")
    }

    case "last" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case l: List[Any] => l.last
      case s: String => s.last
      case Literal(s) => s.last
      case _ => throw new TypeError("can't get tail of non-list")
    }

    case "init" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case l: List[Any] => l.init
      case s: String => s.init
      case Literal(s) => s.init
      case _ => throw new TypeError("can't get tail of non-list")
    }

    case "cons" => argCount(l, 2); Interpreter.eval(l(2), env) match {
      case list: List[Any] => Interpreter.eval(l(1), env) :: list
      case _ => throw new TypeError("can't cons to non-list")
    }

    case "append" => l.tail.eval(env).flatMap {
      case l: List[Any] => l
      case _ => throw new TypeError("can't append non-lists")
    }

    case "list" => l.tail.eval(env)

    case "shuffle" => argCount(l, 1); Interpreter.eval(l(1), env) match {
      case list: List[Any] => util.Random.shuffle(list)
      case _ => throw new TypeError("can't shuffle a non-list")
    }

    case "print" => println(l.tail.eval(env).mkString)

    // Input from other elements in the craft

    case "input" => {
      if (env.machine != null) env.machine.input else List(14L) // for testing
    }

    // Output to other elements in the craft

    case "output" => argCount(l, 1); ; Interpreter.eval(l(1), env) match {
      case l: List[Any] => if (env.machine != null) env.machine.output(l)
      case _ => throw new TypeError("only lists can be output")
    }

    case "dump-env" => println(env)
  }
}
