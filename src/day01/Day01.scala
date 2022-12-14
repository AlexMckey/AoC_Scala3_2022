package day01

import AoC_Lib.*
import Inputs.*

import javax.swing.border.TitledBorder

object Day01 extends aocd.Problem(2022, 1, Title = "Calorie Counting"):
  def run(input: String): Unit =
    val things = prep(input)
    part1(things)
    part2(things)
    ()

  def prep(input: String): Seq[Int] =
    time("\tprep", {
      input
        .splitByBlankLines
        .map(_.toInts.sum)
        .sorted(Ordering.Int.reverse)
    })

  def part1(foods: Seq[Int]): Int = part1 {
    foods.head
  }

  def part2(foods: Seq[Int]): Int = part2 {
    foods.take(3).sum
  }