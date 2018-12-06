import scala.io.Source

object AOC02 {

  def main(args: Array[String]) = {
    val ids = Source.fromResource("aoc02.txt").mkString.split("\n")

    val part1 = ids
      .map(_.toCharArray)
      .map(_.groupBy(identity))
      .map(_.mapValues(_.length))
      .map(_.toList.map(_._2).distinct)

    val _2 = part1.count(_.contains(2))
    val _3 = part1.count(_.contains(3))

    println(s"Part 1 result => ${_2} * ${_3} = ${_2 * _3}")

    val part2 = ids
      .combinations(2)
      .filter {
        case Array(s1, s2) => checkIfThereIsOnlyOneCharacterDifference(s1, s2)
      }
      .toList
      .headOption
      .map { pair =>
        getCommonCharacters(pair.head, pair.last)
      }

    print(s"Part 2 result => ${part2.getOrElse("not found")}")
  }

  def checkIfThereIsOnlyOneCharacterDifference(s1: String,
                                               s2: String): Boolean =
    s1.zip(s2).count { case (c1, c2) => c1 != c2 } == 1

  def getCommonCharacters(s1: String, s2: String) =
    s1.zip(s2).collect { case (c1, c2) if c1 == c2 => c1 }.mkString
}
