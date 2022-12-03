package day00

import AoC_Lib._

object Day00 extends aocd.Problem(2020, 1):
  def run(input: String): Unit =
    // First, we do some prep work on our input.
    val things = prep(input)
    // Then, run part 1.
    part1(things)
    // Finally, run part 2.
    part2(things)
    ()
    
  def prep(input: String): List[Char] =
    time("prep", {
    // This takes a long time.
    Thread.sleep(2000)
    input.toList
  })

  def part1(chars: List[Char]): Int = part1 {
    // Part 1 is fast.
    Thread.sleep(50)
    chars.size
  }

  def part2(chars: List[Char]): Int = part2 {
    // Part 2 is kinda slow.
    Thread.sleep(500)
    chars.size
  }
