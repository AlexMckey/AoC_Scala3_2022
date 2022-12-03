package day02

import AoC_Lib._
import Inputs._

import scala.language.implicitConversions
import Numeric.Implicits._


object Day02 extends aocd.Problem(2022, 2, Title = "Rock Paper Scissors"):
  def run(input: String): Unit =
    val games = prep(input)
    part1(games)
    part2(games)
    ()

  val win: Map[String, String] = Map("A" -> "C", "B" -> "A", "C" -> "B")

  case class Round(a: String, b: String):
    def calcScore: Int =
      val s1 = if a == b then 3
      else if win(b) == a then 6
      else 0
      val s2 = "ABC".indexOf(b) + 1
      s1 + s2

  extension (s: String)
    private def toRound: Round =
      val s"$a $b" = s: @unchecked
      Round(a, b)

  private def decodeStep1(r: Round): Round =
    Round(r.a, r.b match
      case "X" => "A"
      case "Y" => "B"
      case "Z" => "C")

  private def decodeStep2(r: Round): Round =
    Round(r.a, r.b match
      case "X" => win(r.a)
      case "Y" => r.a
      case "Z" => win.map(_.swap)(r.a))

  def prep(input: String): Seq[Round] =
    time("\tprep", {
      input
        .toStrs
        .map(_.toRound)
    })
    //"A Y\nB X\nC Z".toStrs.map(_.words)

  private def part(games: Seq[Round], decode: Round => Round): Int =
    games
      .map(decode)
      .map(_.calcScore)
      .sum

  def part1(games: Seq[Round]): Int = part1 {
    part(games, decodeStep1)
  }

  def part2(games: Seq[Round]): Int = part2 {
    part(games, decodeStep2)
  }