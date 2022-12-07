package day05

import AoC_Lib._
import Inputs._

object Day05 extends aocd.Problem(2022, 5, Title = "Supply Stacks"):
  def run(input: String): Unit =
    val things = prep(input)
    part1(things)
    part2(things)
    ()

  def prep(input: String): (Array[String], Array[(Int,Int,Int)]) =
    time("\tprep", {
      val Seq(configPart, rearrangementPart): Seq[String] = input.splitByBlankLines
      val stacks = configPart.split("\n")
        .dropRight(1)
        .map(_.grouped(4).map(_.drop(1).head).toArray)
        .transpose
        .map(_.dropWhile(_ == ' ').mkString)
      val rearrangement = rearrangementPart.split("\n")
        .map { case s"move $cnt from $from to $to": String =>
          (cnt.toInt, from.toInt-1, to.toInt-1)
        }
      stacks -> rearrangement
    })

  def part(stacks: Array[String],
           rearrangement: Array[(Int,Int,Int)],
           crane: String => String = identity): String =
    rearrangement.foldLeft(stacks) { case (state, (amount, from, to)) =>
      val (prefix, suffix) = state(from).splitAt(amount)
      val result = crane(prefix)
      state.updated(from, suffix).updated(to, result + state(to))
    }
      .map(_.head).mkString

  def part1(data: (Array[String], Array[(Int,Int,Int)])): String = part1 {
    part(data._1, data._2, _.reverse)
  }

  def part2(data: (Array[String], Array[(Int,Int,Int)])): String = part2 {
    part(data._1, data._2)
  }