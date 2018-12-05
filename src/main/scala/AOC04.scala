import scala.io.Source

case class Event(hour: Int, minute: Int, `type`: String, guardId: Int)

object AOC04 {

  val basic = "\\[(\\d{4})-(\\d{2})-(\\d{2})\\s(\\d{2}):(\\d{2})\\]\\s(.*)".r
  val shift = "Guard #(\\d+) begins shift".r
  val sleep = "falls asleep".r

  def main(args: Array[String]) = {
    val lines = Source.fromResource("aoc04.txt").mkString.split("\n").sorted
    val events: Seq[Event] = parseFile(lines)
    val guardSleepAndAwakes = events
      .filter(!_.`type`.equals("begin"))
      .filter(_.hour == 0)
      .groupBy(_.guardId)

    val foo = guardSleepAndAwakes.mapValues { guardEvents =>
      val sleeps = guardEvents.indices.collect {
        case i if i % 2 == 0 => guardEvents(i)
      }.toList
      val awakes = guardEvents.indices.collect {
        case i if i % 2 == 1 => guardEvents(i)
      }.toList
      sleeps.zip(awakes).map {
        case (start, end) =>
          Range(start.minute, end.minute - 1)
      }
    }

    val part1 = foo.toList.maxBy {
      case (_, sleepRanges) =>
        sleepRanges.foldLeft(0)((acc, range) => acc + range.size)
    }

    println(
      s"Part one: ${part1._1} ${mostFrequentMinute(part1._2)} => ${part1._1 * mostFrequentMinute(part1._2)._1}"
    )

    val part2 = foo.toList.maxBy {
      case (_, sleepRanges) => mostFrequentMinute(sleepRanges)._2
    }

    println(
      s"Part two: ${part2._1} ${mostFrequentMinute(part2._2)} => ${part2._1 * mostFrequentMinute(part2._2)._1}"
    )

  }

  def mostFrequentMinute(ranges: List[Range]): (Int, Int) = {
    ranges
      .foldLeft(List[Int]())((acc, range) => acc ++ range.toList)
      .groupBy(identity)
      .mapValues(_.size)
      .toList
      .maxBy(_._2)
  }

  def parseFile(lines: Array[String]): Seq[Event] = {
    val events = lines.foldLeft(Seq[Event]())((events, line) => {
      val event =
        prepareEvent(line, if (events.isEmpty) 0 else events.last.guardId)
      events :+ event
    })
    events
  }

  def prepareEvent(line: String, lastGuardId: Int): Event = line match {
    case AOC04.basic(_, _, _, hour, minute, rest) =>
      if (AOC04.shift.findFirstMatchIn(rest).isDefined) {
        Event(
          hour.toInt,
          minute.toInt,
          "begin",
          AOC04.shift.findFirstMatchIn(rest).get.group(1).toInt
        )
      } else if (AOC04.sleep.findFirstMatchIn(rest).isDefined) {
        Event(hour.toInt, minute.toInt, "sleep", lastGuardId)
      } else {
        Event(hour.toInt, minute.toInt, "awake", lastGuardId)
      }
  }
}
