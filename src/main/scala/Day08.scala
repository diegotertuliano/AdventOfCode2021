import cats.data.NonEmptyList
import cats.parse.Parser
import cats.parse.Parser.char as pchar
import cats.parse.Rfc5234.{alpha, digit, sp}

import scala.io.Source
import scala.util.Using

object Day08:
  val numberParser: Parser[(NonEmptyList[String], NonEmptyList[String])] = {
    val stringParser: Parser[String] = alpha.rep.string
    val digits: Parser[NonEmptyList[String]] = (stringParser <* sp.?).rep
    ((digits <* pchar('|') <* sp) ~ digits)
  }

  def part1(patternsWithOutputs: List[(NonEmptyList[String], NonEmptyList[String])]): Int =
    patternsWithOutputs.map { case (patterns, output) =>
      output
        .map { digit =>
          if ((digit.length == 2) || (digit.length == 3) || (digit.length == 4) || (digit.length == 7))
            1
          else
            0
        }
        .toList
        .sum
    }.sum

  def part2(patternsWithOutputs: List[(NonEmptyList[String], NonEmptyList[String])]): Int =
    patternsWithOutputs.map { case (patterns, output) =>
      val knownNumbers = patterns.toList.flatMap { digit =>
        digit.length match {
          case 2 => Some(1 -> digit.toSet)
          case 3 => Some(7 -> digit.toSet)
          case 4 => Some(4 -> digit.toSet)
          case 7 => Some(8 -> digit.toSet)
          case _ => None
        }
      }.toMap

      val patternsSets: Map[Int, List[Set[Char]]] = patterns.foldLeft(Map.empty[Int, List[Set[Char]]]) {
        case (acc, digit) =>
          acc.updatedWith(digit.length) {
            case Some(value) => Some(acc(digit.length) :+ digit.toSet)
            case None        => Some(List(digit.toSet))
          }
      }

      val digitsMapping: Map[Set[Char], Int] = patternsSets.foldLeft(Map.empty[Set[Char], Int]) {
        case (acc, patterns) =>
          val digitsMapping = patterns match {
            case (5, list) =>
              list.map { d =>
                if (knownNumbers(7).subsetOf(d))
                  d -> 3
                else if (knownNumbers(4).intersect(d).size == 3)
                  d -> 5
                else
                  d -> 2
              }.toMap
            case (6, list) =>
              list.map { d =>
                if (knownNumbers(4).subsetOf(d))
                  d -> 9
                else if (knownNumbers(7).subsetOf(d))
                  d -> 0
                else
                  d -> 6
              }.toMap
            case (2, list) => Map(list.head -> 1)
            case (3, list) => Map(list.head -> 7)
            case (4, list) => Map(list.head -> 4)
            case (7, list) => Map(list.head -> 8)
          }
          acc ++ digitsMapping
      }

      output.toList.map(a => digitsMapping(a.toSet)).reverse.zipWithIndex.foldLeft(0) { case (acc, (n, i)) =>
        acc + (n * math.pow(10, i).toInt)
      }
    }.sum

  def main(args: Array[String]): Unit =
    val fileName: String = "input/Day08"

    val patterns_with_outputs = Using
      .resource(Source.fromFile(fileName)) { source =>
        source.getLines.toList
          .map {
            numberParser.parse
          }
      }
      .collect { case Right((_, (l1, l2))) =>
        (l1, l2)
      }

    println(part1(patterns_with_outputs))
    println(part2(patterns_with_outputs))
