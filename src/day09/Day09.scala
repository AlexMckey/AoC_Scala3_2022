package day09

import AoC_Lib._
import Inputs._

import AoC_Lib.Pos

object Day09 extends aocd.Problem(2022, 9, Title = "Rope Bridge"):
  def run(input: String): Unit =
    val things = prep(input)
    part1(things)
    part2(things)
    ()

  def prep(input: String): String =
    time("\tprep", {
      input
        .toStrs
        .map { case s"$d $c" => d*c.toInt }
        .mkString
    })

  def move(h: Pos, t: Pos): Pos =
    val d = h - t
    if d.x.abs <= 1 && d.y.abs <= 1
    then t
    else t + Pos(d.x.sign, d.y.sign)

  def go(cmd: Char, h: Pos): Pos =
    h + (cmd match
      case 'R' => Pos(1, 0)
      case 'L' => Pos(-1, 0)
      case 'U' => Pos(0, 1)
      case 'D' => Pos(0, -1))

  def calcTailPath(rope: List[Pos], path: String): Set[Pos] =
    path
      .scanLeft(rope) { case (r, cmd) =>
        r.tail.scanLeft(go(cmd, r.head))(move) }
      .map(_.last)
      .toSet

  def part1(path: String): Int = part1 {
    val rope = List(Pos.zero, Pos.zero)
    calcTailPath(rope, path).size
  }

  def part2(path: String): Int = part2 {
    val rope = List.fill(10){ Pos.zero }
    calcTailPath(rope, path).size
  }