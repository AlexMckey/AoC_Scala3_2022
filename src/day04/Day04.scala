package day04

import AoC_Lib.*
import Inputs.*

object Day04 extends aocd.Problem(2022, 4, Title = "Camp Cleanup"):
  def run(input: String): Unit =
    val things = prep(input)
    part1(things)
    part2(things)
    ()

  def prep(input: String): Seq[(Pos,Pos)] =
    time("\tprep", {
      input
        .toStrs
        .map(_.split(",").flatMap(_.split("-"))
          .map(_.toInt))
        .map { case Array(a, b, c, d) => Pos(a, b) -> Pos(c, d) }
      //.map { s =>
      //   val s"$a-$b,$c-$d" = s: @unchecked
      //   Pos(a.toInt, b.toInt) -> Pos(c.toInt, d.toInt)}
    })

  def fullContain(p1: Pos, p2: Pos): Boolean =
    (p1.x >= p2.x && p1.y <= p2.y) ||
      (p2.x >= p1.x && p2.y <= p1.y)

  def overlap(p1: Pos, p2: Pos): Boolean =
    (p2.x >= p1.x && p2.x <= p1.y) ||
      (p2.y >= p1.x && p2.y <= p1.y) ||
      (p1.x >= p2.x && p1.x <= p2.y) ||
      (p1.y >= p2.x && p1.y <= p2.y)

  def part1(pairs: Seq[(Pos,Pos)]): Int = part1 {
    pairs.count(fullContain)
  }

  def part2(pairs: Seq[(Pos,Pos)]): Int = part2 {
    pairs.count(overlap)
  }