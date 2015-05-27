
package com.flowlikeariver.kakuro

abstract sealed class Cell

case class Empty() extends Cell
case class Down(down: Int) extends Cell
case class Across(across: Int) extends Cell
case class DownAcross(down: Int, across: Int) extends Cell
case class Value(values: Set[Int]) extends Cell

object Kakuro {

  def e() = Empty()
  def d(down: Int) = Down(down)
  def a(across: Int) = Across(across)
  def da(down: Int, across: Int) = DownAcross(down, across)
  def v() = Value(Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
  def v(values: Set[Int]) = Value(values)

  def draw(cell: Cell): String = cell match {
    case Empty()          => "   -----  "
    case Down(n)          => "   %2d\\--  ".format(n)
    case Across(n)        => "   --\\%2d  ".format(n)
    case DownAcross(d, a) => "   %2d\\%2d  ".format(d, a)
    case Value(vs)        => " xxxxxxxxx"
  }

  def main(args: Array[String]) {
    println("Hello, world!")
  }
}

