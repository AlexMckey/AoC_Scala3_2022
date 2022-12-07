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

  def prep(input: String): Seq[String] =
    time("\tprep", { input.toStrs })

  def part1(ops: Seq[String]): Int = part1 {
    val strengthCycles = List(20,60,100,140,180,220)
    val cyclesRegs = ops.scanLeft(0 -> 1) { case ((cycle, reg), op) =>
      op match
        case s"noop" => (cycle + 1) -> reg
        case s"addx $x" => (cycle + 2) -> (reg + x.toInt)
    }
    val res = strengthCycles
      .map(fc => cyclesRegs
        .find(c => c._1 == fc - 1 || c._1 == fc - 2))
      .map(_.map(_._2)
        .getOrElse(1))
    strengthCycles
      .zip(res)
      .map(_ * _)
      .sum
  }

  def part2(ops: Seq[String]): String = part2 {
    val sb = new StringBuilder(240)
    ops.foldLeft(0, 1) { case ((c, r), op) =>
      op match
        case s"noop" =>
          val newc = c + 1
          val newr = r
          if c % 40 >= r - 1 && c % 40 <= r + 1 then sb.append("#") else sb.append(" ")
          (newc, newr)
        case s"addx $d" =>
          val newc = c + 2
          val newr = r + d.toInt
          if c % 40 >= r - 1 && c % 40 <= r + 1 then sb.append("#") else sb.append(" ")
          if (c + 1) % 40 >= r - 1 && (c + 1) % 40 <= r + 1 then sb.append("#") else sb.append(" ")
          (newc, newr)
    }
    sb.toString().grouped(40).mkString("\n","\n","")
  }