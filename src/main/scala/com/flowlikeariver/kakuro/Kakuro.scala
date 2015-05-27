
package com.flowlikeariver.kakuro

abstract sealed class Cell

case class Empty() extends Cell
case class Down(down: Int) extends Cell
case class Across(across: Int) extends Cell
case class DownAcross(down: Int, across: Int) extends Cell
case class Value(values: Set[Int]) extends Cell

object Kakuro {

  def e() = Empty()
  def dd(down: Int) = Down(down)
  def aa(across: Int) = Across(across)
  def da(down: Int, across: Int) = DownAcross(down, across)
  def v() = Value(Set(1, 2, 3, 4, 5, 6, 7, 8, 9))
  def v(values: Set[Int]) = Value(values)

  def draw(cell: Cell): String = cell match {
    case Empty()          => "   -----  "
    case Down(n)          => "   %2d\\--  ".format(n)
    case Across(n)        => "   --\\%2d  ".format(n)
    case DownAcross(d, a) => "   %2d\\%2d  ".format(d, a)
    case Value(vs) =>
      if (1 == vs.size) {
        vs.map(x => "     " + "%d".format(x) + "    ").mkString("")
      } else {
        " " + List(1, 2, 3, 4, 5, 6, 7, 8, 9).map(x => if (vs(x)) "%d".format(x) else ".").mkString("")
      }
  }

  def drawRow(row: List[Cell]) = row.map(c => draw(c)).mkString("") + "\n"

  def drawGrid(grid: List[List[Cell]]) = "\n" + grid.map(r => drawRow(r)).mkString("")

  val grid1 = List(List(e, dd(4), dd(22), e, dd(16), dd(3)))

  def allDifferent(nums: List[Int]): Boolean = (nums.size == nums.toSet.size)

  def permute(vs: List[Cell], target: Int, soFar: List[Int]): List[List[Int]] = {
    if (target >= 1) {
      if (soFar.size == (vs.size - 1)) {
        List(soFar ++ List(target))
      } else {
        vs(soFar.size) match {
          case Value(vset) => vset.toList.flatMap(v => permute(vs, (target - v), soFar ++ List(v)))
          case _           => List()
        }
      }
    } else {
      List()
    }
  }

  def permuteAll(vs: List[Cell], total: Int): List[List[Int]] = permute(vs, total, List())

  def isPossible(cell: Cell, n: Int) = cell match {
    case Value(values) => values(n)
    case _             => false
  }

  def transpose[T](matrix: List[List[T]]): List[List[T]] = matrix match {
    case row :: rows =>
      row match {
        case col :: cols =>
          val first = matrix.map(x => x.head)
          val rest = transpose(matrix.map(x => x.tail))
          first :: rest
        case _ => List()
      }
    case _ => List()
  }

  def solveStep(cells: List[Cell], total: Int) = {
    val last = cells.length - 1
    transpose(permuteAll(cells, total)
              .filter(p => isPossible(cells(last), p(last)))
              .filter(p => allDifferent(p)))
    .map(p => Value(p.toSet))
  }

  def rowTarget(cell: Cell) = cell match {
    case Across(n)        => n
    case DownAcross(_, a) => a
    case _                => 0
  }

  def colTarget(cell: Cell) = cell match {
    case Down(d)          => d
    case DownAcross(d, _) => d
    case _                => 0
  }

  def solvePair(f: Cell => Int, pair: List[List[Cell]]) = pair match {
    case nvs :: nil => nvs
    case nvs :: vs :: _ => (nvs ++ solveStep(vs, f(nvs.last)))
    case _ => List()
  }

  def main(args: Array[String]) {
    println("Hello, world!")
  }
}

