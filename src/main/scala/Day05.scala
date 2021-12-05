import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Parser.{char => pchar}
import cats.parse.Rfc5234.{digit, sp}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day05:

  type GridType = Array[Array[Int]]
  case class Coordinate(x: Int, y: Int)

  val coordinatesParser: Parser[(Coordinate, Coordinate)] = {
    val coordinatePair: Parser[Coordinate] = ((digit.rep.string <* pchar(',')) ~ digit.rep.string).map { case (x, y) =>
      Coordinate(x.toInt, y.toInt)
    }
    val arrow = (pchar('-') ~ pchar('>')).surroundedBy(sp)

    (coordinatePair <* arrow) ~ coordinatePair
  }

  def part1(coordinatePairs: List[(Coordinate, Coordinate)]): Int =
    val gridSize = 1000
    val grid: GridType = Array.fill(gridSize)(Array.fill(gridSize)(0))
    var overlap = 0

    coordinatePairs.foreach { case (start, end) =>
      val begin = Coordinate(Math.min(start.x, end.x), Math.min(start.y, end.y))
      val finish = Coordinate(Math.max(start.x, end.x), Math.max(start.y, end.y))

      if (begin.x == finish.x) {
        for (y <- begin.y to finish.y) {
          grid(y)(begin.x) += 1
          if (grid(y)(begin.x) == 2) overlap += 1
        }
      } else if (begin.y == finish.y) {
        for (x <- begin.x to finish.x) {
          grid(begin.y)(x) += 1

          if (grid(begin.y)(x) == 2) overlap += 1
        }
      }
    }

    overlap

  def part2(coordinatePairs: List[(Coordinate, Coordinate)]): Int =
    val gridSize = 1000
    val grid: GridType = Array.fill(gridSize)(Array.fill(gridSize)(0))
    var overlap = 0

    coordinatePairs.foreach { case (start, end) =>
      val differenceX = end.x - start.x
      val intervalX = math.abs(differenceX)
      val unitIntervalX = differenceX.sign

      val differenceY = end.y - start.y
      val intervalY = math.abs(differenceY)
      val unitIntervalY = differenceY.sign

      for (d <- 0 to math.max(intervalX, intervalY)) {
        grid(start.y + (d * unitIntervalY))(start.x + (d * unitIntervalX)) += 1

        if (grid(start.y + (d * unitIntervalY))(start.x + (d * unitIntervalX)) == 2) overlap += 1
      }

    }

    overlap

  def main(args: Array[String]): Unit =
    val fileName: String = "input/Day05"

    val coordinatePairs = Using
      .resource(Source.fromFile(fileName)) { source =>
        source.getLines.toList.map(coordinatesParser.parse)
      }
      .collect { case Right((_, coordinates)) =>
        coordinates
      }

    println(part1(coordinatePairs))

    println(part2(coordinatePairs))
