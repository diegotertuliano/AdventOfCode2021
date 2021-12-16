import cats.parse.Parser
import cats.parse.Rfc5234.char

import scala.collection.mutable
import scala.io.Source
import scala.util.Using

object Day10 {
  val stringParser: Parser[String] = char.rep.string

  val opening = Map('(' -> 1, '[' -> 2, '{' -> 3, '<' -> 4)
  val closing = Map(')' -> 3, ']' -> 57, '}' -> 1197, '>' -> 25137)
  val matching = Map(')' -> '(', ']' -> '[', '}' -> '{', '>' -> '<')

  def solve(lines: List[String]): List[(Int, Long)] =
    def parse(line: String): (Int, Long) = {
      val stack = mutable.Stack[Char](line(0))
      line
        .substring(1)
        .foreach(c =>
          if opening.contains(c) then stack.push(c)
          else
            val last = stack.pop()
            if last != matching(c) then return (closing(c), 0)
        )
      if stack.nonEmpty then
        val totalScore = stack.foldLeft(0L)((total, next) => 5 * total + opening(next))
        (0, totalScore)
      else (0, 0)
    }
    lines.map { line => parse(line) }

  def main(args: Array[String]): Unit =
    val fileName: String = "input/Day10"

    val lines = Using
      .resource(Source.fromFile(fileName)) { source =>
        source.getLines.toList.map(stringParser.parse)
      }
      .collect { case Right((_, line)) =>
        line
      }

    println(solve(lines).map { _._1 }.sum)

    val totalScores = solve(lines).map { _._2 }.filter(_ != 0).sorted
    println(totalScores(totalScores.length / 2))
}
