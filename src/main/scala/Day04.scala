import cats.data.NonEmptyList
import cats.parse.{Numbers, Parser}
import cats.parse.Rfc5234.{digit, sp}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Using

object Day04:
  val numbersParsingWithCommas: Parser[NonEmptyList[Int]] =
    (digit.rep.string <* Parser.char(',').?).rep.map(_.map(_.toInt))
  val numbersParsingWithSpaces: Parser[NonEmptyList[Int]] =
    (sp.?.with1 *> digit.rep.string <* sp.rep.?).rep.map(_.map(_.toInt))

  type BoardType = Vector[Vector[Int]]

  case class Board(board: BoardType):
    def markDrawNumbers(drawNumber: Int): Board =
      Board(board.map(_.map(n => if (n == drawNumber) 0 else n)))

    def isNotBoardWinner: Boolean =
      !(board.exists(_.forall(n => n == 0)) || board.transpose.exists(_.forall(n => n == 0)))

  def part1(drawNumbers: List[Int], boards: List[Board]): Int =
    @tailrec
    def go(drawNumber: Int, boards: List[Board], idx: Int): Int =
      val markedBoards = boards.map(_.markDrawNumbers(drawNumber))
      val boardWinner = markedBoards.dropWhile(_.isNotBoardWinner)

      if (boardWinner.isEmpty) go(drawNumbers(idx + 1), markedBoards, idx + 1)
      else {
        val sum = boardWinner.head.board.map(_.sum).sum
        drawNumber * sum
      }

    go(drawNumbers.head, boards, 0)

  def part2(drawNumbers: List[Int], boards: List[Board]): Int =
    @tailrec
    def go(drawNumber: Int, boards: List[Board], idx: Int): Int =
      val markedBoards = boards.map(_.markDrawNumbers(drawNumber))
      val loserBoards = markedBoards.filter(_.isNotBoardWinner)

      if (loserBoards.isEmpty) {
        val sum = markedBoards.head.board.map(_.sum).sum
        drawNumber * sum
      } else go(drawNumbers(idx + 1), loserBoards, idx + 1)

    go(drawNumbers.head, boards, 0)

  def main(args: Array[String]): Unit =
    val fileName: String = "input/Day04"

    val (drawNumbers, boards) = Using
      .resource(Source.fromFile(fileName)) { source =>
        val it = source.getLines()

        val drawNumbers = numbersParsingWithCommas.parse(it.next())
        val boards = it.toList
          .filter(_.nonEmpty)
          .map(numbersParsingWithSpaces.parse)
          .collect { case Right((_, boardRow)) => boardRow.toList.toVector }
          .toVector
          .grouped(5)
          .map(Board.apply)
          .toList

        (drawNumbers, boards)
      }

    drawNumbers.foreach { case (_, drawNumbers) =>
      println(part1(drawNumbers.toList, boards))
      println(part2(drawNumbers.toList, boards))
    }
