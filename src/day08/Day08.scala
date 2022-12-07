package day08

import AoC_Lib._
import Inputs._

object Day08 extends aocd.Problem(2022, 8, Title = "Treetop Tree House"):
  def run(input: String): Unit =
    val things = prep(input)
    part1(things)
    part2(things)
    ()

  def prep(input: String): Array[Array[Int]] =
    time("\tprep", {
      input
        .toStrs
        .map(_.map(_.asDigit).toArray)
        .toArray
    })

  def part1(treeMap: Array[Array[Int]]): Int = part1 {

    import math.max

    val il = treeMap.length - 1
    val jl = treeMap.head.length - 1

    val fl = treeMap.map(_.scanLeft(-1)(max))
    val fr = treeMap.map(_.scanRight(-1)(max))
    val fu = treeMap.transpose.map(_.scanLeft(-1)(max))
    val fd = treeMap.transpose.map(_.scanRight(-1)(max))

    val visibility = for
      i <- 0 to il
      j <- 0 to jl
      tree = treeMap(i)(j)
      l = tree > fl(i)(j)
      r = tree > fr(i)(j + 1)
      u = tree > fu(j)(i)
      d = tree > fd(j)(i + 1)
      if l || r || d || u
    yield 1

    visibility.sum
  }

  def part2(treeMap: Array[Array[Int]]): Int = part2 {

    val il = treeMap.length - 1
    val jl = treeMap.head.length - 1

    extension (n: Int)
      def orNotFound(x: Int): Int = if n < 0 then x else n

    val score =
      for
        i <- 0 to il
        j <- 0 to jl
        tree = treeMap(i)(j)
        l = j - treeMap(i).lastIndexWhere(_ >= tree, j - 1).orNotFound(0)
        r = treeMap(i).indexWhere(_ >= tree, j + 1).orNotFound(jl) - j
        u = i - treeMap.lastIndexWhere(l => l(j) >= tree, i - 1).orNotFound(0)
        d = treeMap.indexWhere(l => l(j) >= tree, i + 1).orNotFound(il) - i
      yield
        l * r * u * d

    score.max
  }