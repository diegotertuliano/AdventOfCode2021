import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Parser.char as pchar
import cats.parse.Rfc5234.digit

import scala.io.Source
import scala.util.Using

object Day06:

  val numberParser: Parser[NonEmptyList[Int]] = (digit.rep.string <* pchar(',').?).rep.map(_.map(_.toInt))

  def simulation(ages: List[Int], day: Int): Long =
    val agesState: Array[Long] = Array.fill(9)(0)
    ages.foreach { age =>
      agesState(age) += 1
    }

    for (day <- 0 until day)
      val newFishes = agesState(0)

      for (i <- 0 until (agesState.length - 1))
        agesState(i) = agesState(i + 1)

      agesState(6) += newFishes
      agesState(8) = newFishes

    agesState.sum

  def main(args: Array[String]): Unit =
    val fileName: String = "input/Day06"

    val initialFishAges = Using
      .resource(Source.fromFile(fileName)) { source =>
        numberParser.parse(source.getLines.next())
      } match {
      case Right((_, initialAge)) =>
        initialAge.toList
    }

    println(simulation(initialFishAges, 80))
    println(simulation(initialFishAges, 256))
