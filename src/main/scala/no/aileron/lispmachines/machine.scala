package no.aileron.lispmachines

import collection.mutable._

class World {
  val machines = Set[Machine]()
  val links = Set[Link]()

  def newLispMachine(name: String): LispMachine = {
    val m = new LispMachine(name, this)
    machines += m
    return(m)
  }

  def newLink(m1: Machine, m2: Machine): Link = {
    val l = new Link(m1, m2)
    links += l
    return(l)
  }

  def output(m: Machine, l: List[Any]) {
    links.filter(_.input == m).head.put(l)
  }

  def input(m: Machine): List[Any] = {
    links.filter(_.output == m).head.get
  }
}

// This is any kind of machine that can send and/or recieve data

class Machine(name: String, world: World) {
  def output(l: List[Any]) { world.output(this, l) }
  def input: List[Any] = world.input(this)
}

// This is a machine that has Lisp code

class LispMachine(name: String, world: World) extends Machine(name, world) {
  val repl = new REPL(this)
}

class Link(val input: Machine, val output: Machine) {
  val buffer = new ListBuffer[List[Any]]()
  def put(l: List[Any]) {
    buffer += l
  }

  def get: List[Any] = {
    val v = buffer.head
    buffer.trimStart(1)
    return v
  }
}
