package day10

import AoC_Lib.*
import Inputs.*

import javax.swing.border.TitledBorder

object Day10 extends aocd.Problem(2022, 10, Title = "Cathode-Ray Tube"):
  def run(input: String): Unit =
    val things = prep(input)
    part1(things)
    part2(things)
    ()

  def prep(input: String): Array[(Int,Int)] =
    time("\tprep", {
      input.
        toStrs
        .foldLeft((0, 1, List.empty[(Int, Int)])) {
          case ((c, r, acc), op) =>
            op match
              case s"noop" => (c + 1, r, (c, r) :: acc)
              case s"addx $x" => (c + 2, r + x.toInt, (c + 1, r) :: (c, r) :: acc)
        }._3
        .reverse
        .toArray
    })

  def part1(cycles: Array[(Int,Int)]): Int = part1 {
    val strengthCycles = List(20,60,100,140,180,220)
    strengthCycles.map(c => c * cycles(c - 1)._2).sum
  }

  def part2(cycles: Array[(Int,Int)]): String = part2 {
    cycles.foldLeft(new StringBuilder(140)) {
      case (sb, (c, r)) =>
        if r - 1 to r + 1 contains c % 40 then sb.append("#") else sb.append(".")
    }.toString()
      .grouped(40)
      .mkString("\n", "\n", "")
  }