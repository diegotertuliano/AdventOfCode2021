import cats.kernel.Monoid.combineAll

import scala.util.Using
import scala.io.Source
import cats.parse.Rfc5234.{alpha, digit, sp}
import cats.parse.{Numbers, Parser}

object Day02 {

  val stringParser: Parser[String] = alpha.rep.string
  val digitParser: Parser[Int] = (sp *> Numbers.digits).map(_.toInt)
  val lineParser: Parser[(String, Int)] = stringParser ~ digitParser

  case class LineInput(movementType: String, movementInterval: Int)

  case class Coordinate(x: Int, y: Int)

  def part1(lines: List[LineInput]): Int =

    val coordinates = lines.foldLeft((0, 0)) { (acc, lineInput) =>
      lineInput.movementType match {
        case "forward" =>
          combineAll(List(acc, (lineInput.movementInterval, 0)))
        case "up" =>
          combineAll(List(acc, (0, -lineInput.movementInterval)))
        case "down" =>
          combineAll(List(acc, (0, lineInput.movementInterval)))
      }
    } match {
      case (x, y) => Coordinate(x, y)
    }

    coordinates.x * coordinates.y

  def part2(lines: List[LineInput]): Int =

    val coordinates = lines.foldLeft((0, 0, 0)) { (acc, lineInput) =>
      lineInput.movementType match {
        case "forward" =>
          acc match {
            case (_, _, aim) =>
              combineAll(List(acc, (lineInput.movementInterval, aim * lineInput.movementInterval, 0)))
          }
        case "up" =>
          combineAll(List(acc, (0, 0, -lineInput.movementInterval)))
        case "down" =>
          combineAll(List(acc, (0, 0, lineInput.movementInterval)))
      }
    } match {
      case (x, y, _) => Coordinate(x, y)
    }

    coordinates.x * coordinates.y

  def main(args: Array[String]): Unit =
    val fileName: String = "input/Day02"

    val lines: List[LineInput] = Using
      .resource(Source.fromFile(fileName)) { source =>
        source.getLines.toList.map(lineParser.parse)
      }
      .collect { case Right(_, (movementType, movementInterval)) =>
        LineInput(movementType, movementInterval)
      }

    println(part1(lines))

    println(part2(lines))

}
