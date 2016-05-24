package no.aileron.lispmachines

import collection.mutable._
import java.util.Calendar

// This is any kind of machine that can send and/or recieve data

class Machine(name: String, world: World) {
  def output(l: List[Any]) { world.output(this, l) }
  def input: List[Any] = world.input(this)
  def tick { /* noop */ }

  world.register(this)
}

// This is a machine that has Lisp code

class LispMachine(name: String, world: World) extends Machine(name, world) {
  val repl = new REPL(this)
  var code: String = ""
  override def tick {
    repl.execute(code)
  }
}

class ClockMachine(name: String, world: World) extends Machine(name, world) {
  override def tick {
    val now = Calendar.getInstance().get(Calendar.SECOND)
    output(List("clock", now))
  }
}

// For debugging
class PrintMachine(name: String, world: World) extends Machine(name, world) {
  override def tick {
    world.links.filter(_.output == this).map(l => while(l.length > 0) println(l.get))
  }
}
