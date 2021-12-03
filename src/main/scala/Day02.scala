import scala.util.Using
import scala.io.Source

import cats.parse.Rfc5234.{sp, alpha, digit}
import cats.parse.{Parser, Numbers}

val stringParser: Parser[String] = alpha.rep.string
val digitParser: Parser[Int] = (sp *> Numbers.digits).map(_.toInt)
val lineParser: Parser[(String, Int)] = stringParser ~ digitParser

def part1(lines: List[Either[Parser.Error, (String, (String, Int))]]): Int =
  var horizontalMovement = 0
  var verticalMovement = 0
  var aim = 0

  lines.collect {
    case Right(_, (movementType, movementInterval)) =>
      movementType match {
        case "forward" =>
          horizontalMovement += movementInterval
        case "up" =>
          verticalMovement -= movementInterval
        case "down" =>
          verticalMovement += movementInterval
      }
  }

  horizontalMovement * verticalMovement

def part2(lines: List[Either[Parser.Error, (String, (String, Int))]]): Int =
  var horizontalMovement = 0
  var verticalMovement = 0
  var aim = 0

  lines.collect {
    case Right(_, (movementType, movementInterval)) =>
      movementType match {
        case "forward" =>
          horizontalMovement += movementInterval
          verticalMovement += aim * movementInterval
        case "up" =>
          aim -= movementInterval
        case "down" =>
          aim += movementInterval
      }
  }
  horizontalMovement * verticalMovement

@main def Day02(): Unit =
  val fileName: String = "input/Day02"

  val lines = Using.resource(Source.fromFile(fileName)) { source =>
    source.getLines.toList.map(lineParser.parse)
  }

  println(part1(lines))

  println(part2(lines))



