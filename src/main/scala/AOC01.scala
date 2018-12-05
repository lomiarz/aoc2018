import scala.io.Source

object AOC01 {

  def main(args: Array[String]) = {
    val longs = Source.fromResource("aoc01.txt").mkString.split("\n").map(_.toLong)

    val part1 = longs.foldLeft(0L)((acc, frequency) => acc + frequency)
    println(s"Part1 => $part1")

    def part2 = Stream
      .continually(longs)
      .flatten
      .scanLeft((0L, Set.empty[Long])) {
        case ((sum, visited), freq) => (sum + freq, visited + sum)
      }
      .find { case (sum, visited) => visited.contains(sum)}
      .get._1

    println(s"Part2 => $part2")
  }
}
