package no.aileron.lispmachines

import collection.mutable._

class World {
  val machines = Set[Machine]()
  val links = Set[Link]()

  def register(m: Machine) {
    machines += m
  }

  def newLink(m1: Machine, m2: Machine): Link = {
    val l = new Link(m1, m2)
    links += l
    return(l)
  }

  def output(m: Machine, l: List[Any]) {
    val outputLinks = links.filter(_.input == m)
    if (outputLinks.size > 0) outputLinks.head.put(l)
  }

  // If there are multiple links to this machine, the "first" link will be emptied before the next is used
  def input(m: Machine): List[Any] = {
    val inputLinks = links.filter(_.output == m)
    if (inputLinks.size > 0) inputLinks.head.get else Nil
  }

  def tick {
    machines.map(_.tick)
  }
}

class Link(val input: Machine, val output: Machine) {
  val buffer = new ListBuffer[List[Any]]()
  def put(l: List[Any]) {
    buffer += l
  }

  def get: List[Any] = {
    if (length == 0) return Nil
    val v = buffer.head
    buffer.trimStart(1)
    return v
  }

  def length = buffer.size
}
