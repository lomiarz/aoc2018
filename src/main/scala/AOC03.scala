import scala.io.Source

case class Claim(id: Long, x: Long, y: Long, wide: Long, tall: Long)

object AOC03 {

  def main(args: Array[String]) = {
    val claims =
      Source.fromResource("aoc03.txt").mkString.split("\n").map(parseLine)

    val positions = for {
      Claim(id, x, y, wide, tall) <- claims
      x <- x until x + wide
      y <- y until y + tall
    } yield (id, (x, y))

    val part1 =
      positions.map(_._2).groupBy(identity).mapValues(_.length).count(_._2 > 1)
    println(s"Part 1 result => $part1")

    val notOverlapping = positions
      .map(_._2)
      .groupBy(identity)
      .mapValues(_.length)
      .filter(_._2 == 1)
      .keySet

    val part2 = positions.groupBy(_._1).mapValues(_.map(_._2)).filter {
      case (_, points) =>
        points.forall(notOverlapping.contains)
    }.head._1

    println(s"Part 2 result => $part2")
  }

  def parseLine(line: String) = {
    val regexp = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r
    line match {
      case regexp(id, x, y, wide, tall) =>
        Claim(id.toLong, x.toLong, y.toLong, wide.toLong, tall.toLong)
    }
  }
}
