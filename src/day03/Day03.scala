package day03

import AoC_Lib._
import Inputs._

object Day03 extends aocd.Problem(2022, 3):
  def run(input: String): Unit =
    val things = prep(input)
    //val things = prep("vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw")
    part1(things)
    part2(things)
    ()

  def prep(input: String): Seq[String] =
    time("prep", {
      input
        .toStrs
    })

  def part(items: Seq[Seq[String]]): Int =
    items
      .map(_.reduce(_ intersect _).head)
      .map(c => if c.isLower then c - 'a' + 1 else c - 'A' + 27)
      .sum


  def part1(rucksacks: Seq[String]): Int = part1 {
    part(rucksacks.map(r => r.grouped(r.length/2).toSeq))
  }

  def part2(rucksacks: Seq[String]): Int = part2 {
    part(rucksacks.grouped(3).toSeq)
  }