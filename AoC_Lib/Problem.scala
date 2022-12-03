package aocd

import os.Path

abstract class Problem(year: Int,
                        day: Int,
                        api: Api = Api,
                        assetsDir: os.Path = Problem.assetsDir):
  val problemDir: Path = assetsDir / year.toString
  val inputData: Path = problemDir / f"input$day%02d.txt"

  if !os.exists(problemDir) then os.makeDir.all(problemDir)

  if !os.exists(inputData)
  then
    val token = Problem.getTokenFrom(Problem.getTokenFile(assetsDir))
    os.write.over(inputData, api.getData(year, day, token).bytes)

  def main(args: Array[String]): Unit = run(os.read(inputData))

  def run(input: String): Unit

  final def part1[A](f: => A): A = time("part 1", f, withResult = true)
  final def part2[A](f: => A): A = time("part 2", f, withResult = true)

  final def time[A](prefix: String = "", block: => A, withResult: Boolean = false): A =
    val start = System.nanoTime()
    val result = block
    val end = System.nanoTime()
    val (took, unit, color) = (end - start).toDouble / 1000000 match
      case ms if ms > 1000 => ((ms / 1000).toString, "s", Console.RED)
      case ms if ms > 100  => (ms.toString, "ms", Console.YELLOW)
      case ms              => (ms.toString, "ms", Console.GREEN)
    val timed = s"$prefix: " + color + s"${"%-3.4s".format(took)}$unit" + Console.RESET
    if withResult
    then println(s"$timed - $result")
    else println(s"$timed")
    result

object Problem:
  val TOKEN_ENV = "AOC_SESSION_TOKEN"

  //val assetsDir = os.home / ".aocd"
  val assetsDir: Path = os.Path("""d:/YandexDisk/DevsExercises/AdventOfCode/PuzzleInput/""")
  //val assetsDir: Path = os.Path("""d:/DevsExercises/AdventOfCode/PuzzleInput/""")

  def getTokenFile(dir: os.Path): os.Path = dir / "token"

  def getTokenFrom(tokenFile: os.Path, envvar: String = TOKEN_ENV): String =
    scala.sys.env.getOrElse(envvar, if (os.exists(tokenFile)) os.read(tokenFile).trim()
    else throw TokenNotFound())
