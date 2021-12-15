import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Rfc5234.{digit, sp}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day09 {

  val digitParser: Parser[NonEmptyList[Int]] =
    digit.rep.map(_.map(_.asDigit))

  val directions: List[(Int, Int)] = List((1, 0), (-1, 0), (0, -1), (0, 1))

  type GridType = Vector[Vector[Int]]
  case class Coordinate(x: Int, y: Int)

  case class Grid(grid: GridType):
    def height: Int = grid.length
    def width: Int = grid.head.length
    def inside(x: Int, y: Int): Boolean = (x > -1) && (x < width) && (y > -1) && (y < height)

  def adjacentPositions(p: Coordinate): List[Coordinate] = directions.map { case (dx, dy) =>
    Coordinate(p.x + dx, p.y + dy)
  }

  def riskLevel(height: Int): Int = 1 + height

  def lowestPoints(grid: Grid): List[Coordinate] =
    (0 until grid.width)
      .flatMap(x =>
        (0 until grid.height).flatMap { y =>
          val isLower = adjacentPositions(Coordinate(x, y)).forall { p =>
            !grid.inside(p.x, p.y) || (grid.inside(p.x, p.y) && (grid.grid(y)(x) < grid.grid(p.y)(p.x)))
          }

          if (isLower) Some(Coordinate(x, y))
          else None
        }
      )
      .toList

  def part1(grid: Grid, lowestPoints: List[Coordinate]): Int = lowestPoints.map { p =>
    riskLevel(grid.grid(p.y)(p.x))
  }.sum

  def part2(grid: Grid, lowestPoints: List[Coordinate]): Int =
    def floodFill(visited: Set[Coordinate], p: Coordinate): Set[Coordinate] =
      if (visited.contains(p) || !grid.inside(p.x, p.y) || grid.grid(p.y)(p.x) == 9) visited
      else adjacentPositions(p).foldLeft(visited + p)(floodFill)

    lowestPoints
      .map { p =>
        floodFill(Set(), p).size
      }
      .sortBy(-_)
      .take(3)
      .product

  def main(args: Array[String]): Unit =
    val fileName: String = "input/Day09"

    val grid: Grid = Using
      .resource(Source.fromFile(fileName)) { source =>
        source
          .getLines()
          .toList
          .filter(_.nonEmpty)
          .map(digitParser.parse)
          .collect { case Right((_, boardRow)) => boardRow.toList.toVector }
          .toVector
          .grouped(100)
          .map(Grid.apply)
          .next()
      }

    val lp = lowestPoints(grid)

    println(part1(grid, lp))
    println(part2(grid, lp))
}
