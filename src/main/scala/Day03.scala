import cats.data.NonEmptyList
import cats.kernel.Monoid.combineAll

import scala.util.Using
import scala.io.Source
import cats.parse.Rfc5234.digit
import cats.parse.{Numbers, Parser}

import scala.annotation.tailrec

object Day03 {

  val stringParser: Parser[String] = digit.rep.string

  def getMostCommonAndLeastCommonBits(lines: List[String]): (String, String) = {
    val halfFileInputLength = Math.ceil(lines.length.toDouble / 2).toInt
    val lineLength = lines.head.length
    val initial = List.fill(lineLength)(0)

    lines
      .foldLeft(initial) { (acc, binaryArray) =>
        {
          acc.zip(binaryArray).map((a: Int, b: Char) => a + b.asDigit)
        }
      }
      .map { count =>
        if (count >= halfFileInputLength)
          ('1', '0')
        else
          ('0', '1')
      }
      .unzip match {
      case (mostCommonBits, leastCommonBits) =>
        (mostCommonBits.mkString, leastCommonBits.mkString)
    }
  }

  def part1(lines: List[String]): Int = {

    getMostCommonAndLeastCommonBits(lines) match {
      case (mostCommonBits, leastCommonBits) =>
        val a = Integer.parseInt(mostCommonBits.mkString, 2)
        val b = Integer.parseInt(leastCommonBits.mkString, 2)
        a * b
    }
  }

  def part2(lines: List[String]): Int = {

    @tailrec
    def go(lines: List[String], index: Int, checkMostCommonBits: Boolean): String = {
      val (mostCommonBits, leastCommonBits) = getMostCommonAndLeastCommonBits(lines)
      val filteredList =
        if (checkMostCommonBits)
          lines.filter(binaryString => binaryString(index) == mostCommonBits(index))
        else
          lines.filter(binaryString => binaryString(index) == leastCommonBits(index))

      if (filteredList.length == 1)
        filteredList.head
      else
        go(filteredList, index + 1, checkMostCommonBits)
    }

    val oxygenGeneratorRating = true
    val co2ScrubberRating = false

    List(oxygenGeneratorRating, co2ScrubberRating).map { checkMostCommonBits =>
      Integer.parseInt(go(lines, 0, checkMostCommonBits), 2)
    }.product
  }

  def main(args: Array[String]): Unit =
    val fileName: String = "input/Day03"

    val lines = Using
      .resource(Source.fromFile(fileName)) { source =>
        source.getLines.toList.map(stringParser.parse)
      }
      .collect { case Right((_, binaryString)) =>
        binaryString
      }

    println(part1(lines))

    println(part2(lines))

}
