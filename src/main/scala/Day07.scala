import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Parser.char as pchar
import cats.parse.Rfc5234.digit

import scala.io.Source
import scala.util.Using

object Day07:
  val numberParser: Parser[NonEmptyList[Int]] = (digit.rep.string <* pchar(',').?).rep.map(_.map(_.toInt))

  def medianCalculator(seq: Seq[Int]): Int = {
    // In order if you are not sure that 'seq' is sorted
    val sortedSeq = seq.sortWith(_ < _)

    if (seq.size % 2 == 1) sortedSeq(sortedSeq.size / 2)
    else {
      val (up, down) = sortedSeq.splitAt(seq.size / 2)
      (up.last + down.head) / 2
    }
  }

  def part1(positions: List[Int]): Int =
    val median = medianCalculator(positions)
    positions.map { position => (position - median).abs }.sum

  def part2(positions: List[Int]): Int =
    val meanCeil = Math.ceil(positions.sum.toDouble / positions.length).toInt
    val meanFloor = Math.floor(positions.sum.toDouble / positions.length).toInt

    val fuel = (position: Int, mean: Int) => {
      val absDiff = (position - mean).abs
      ((1 + absDiff) * absDiff) / 2
    }

    positions.map { position =>
      (fuel(position, meanCeil), fuel(position, meanFloor))
    }.unzip match { case (l1, l2) => (l1.sum, l2.sum) } match {
      case (a, b) => Math.min(a, b)
    }

  def main(args: Array[String]): Unit =
    val fileName: String = "input/Day07"

    val positions = Using
      .resource(Source.fromFile(fileName)) { source =>
        numberParser.parse(source.getLines.next())
      } match {
      case Right((_, position)) =>
        position.toList
    }

    println(part1(positions))
    println(part2(positions))
