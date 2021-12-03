import scala.util.Using
import scala.io.Source

@main def Day01(): Unit =
  val fileName: String = "input/Day01"
  val numbers = Using.resource(Source.fromFile(fileName)) { source =>
    source.getLines.toList.map(_.toInt)
  }

  def largerThanPrevious(numbers: List[Int]): Int =
    numbers.sliding(2).foldLeft(0) {
      (accumulator, consecutiveNumbers) =>
        consecutiveNumbers match
          case List(current, next) =>
            if (current < next) accumulator + 1
            else accumulator
          case _ => ???
    }

  val sumLargerThanPreviousTwoMeasurements = largerThanPrevious(numbers)

  val sumLargerThanPreviousThreeMeasurements = largerThanPrevious(numbers.sliding(3).map(_.sum).toList)

  println(sumLargerThanPreviousTwoMeasurements)

  println(sumLargerThanPreviousThreeMeasurements)