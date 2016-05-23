package no.aileron.lispmachines

// import scala.tools.jline
// import org.clapper.argot._
// import java.io.File
// import java.io.PrintWriter

class Env(parent: Env, val machine: Machine) {

  def this(parent: Env) {
    this(parent, null)
  }

  val map: collection.mutable.Map[String, Any] = collection.mutable.Map[String, Any]()


  override def toString = map.mkString("\n")

  def update(k: String, v: Any) {
    map.get(k) match {
      case Some(_) => map(k) = v
      case None => parent match {
        case null => throw new VariableNotFound(k)
        case p => p(k) = v
      }
    }
  }
  def define(k: String, v: Any) {
    v match {
      case f: Function =>
        map.get(k) match {
          case Some(t: FunctionTable) =>
            t.add(f)
          case _ =>
            val t = new FunctionTable()
            t.add(f)
            map(k) = t
        }
      case _ => map(k) = v
    }
  }
  def apply(k: String): Option[Any] = map.get(k) match {
    case None => parent match {
      case null => None
      case _ => parent(k)
    }
    case v => v
  }

  def getFunction(k: String, arity: Int): Option[Function] = map.get(k) match {
    case None => parent match {
      case null => None
      case _ => parent.getFunction(k, arity)
    }
    case Some(t: FunctionTable) =>
      t(arity) match {
      case None => parent match {
        case null => None
        case _ => parent.getFunction(k, arity)
      }
      case f => f
    }
    case _ => None
  }
}

class REPL(machine: Machine) {

  def this() {
    this(null)
  }

  val defaultEnv = new Env(null, machine) {
    override val map = collection.mutable.Map[String, Any](
      "true" -> true, "false" -> false, "unit" -> (),
      "+" -> "+", "-" -> "-", "*" -> "*", "/" -> "/", "%" -> "%",
      ">" -> ">", ">=" -> ">=", "<" -> "<", "<=" -> "<=",  "=" -> "=",  "!=" -> "!=",
      "min" -> "min", "max" -> "max"
    )
  }

  // load builtins defined in lisp
  execute(io.Source.fromFile("builtins.lisp").mkString)

  def execute(l: String) = {
    val ast = LispParser.parse(l.replaceAll(";[^\n$]*", " ").replace("\n", " "))
    Preprocessor.process(ast).map(e => Interpreter.eval(e, defaultEnv))
  }

  def executeLine(l: String) = {
    val r = execute(l)
    r match {
      case l : List[Any] => {
        if(l.length < 1) () else l.last
      }
      case _ => ()
    }
  }
}
