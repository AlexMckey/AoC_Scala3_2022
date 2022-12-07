package day06

import AoC_Lib._
import Inputs._

object Day06 extends aocd.Problem(2022, 6, Title = "Tuning Trouble"):
  def run(input: String): Unit =
    val things = prep(input)
    part1(things)
    part2(things)
    ()

  def prep(input: String): String =
    time("\tprep", {
      input
    })

  def part(data: String, cnt: Int): Int =
    data.sliding(cnt).indexWhere(s => s.distinct.length == cnt) + cnt

  def part1(data: String): Int = part1 {
    part(data,4)
  }

  def part2(data: String): Int = part2 {
    part(data,14)
  }