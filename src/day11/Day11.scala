package day11

import AoC_Lib.*
import Inputs.*

import javax.swing.border.TitledBorder
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.matching.Regex

object Day11 extends aocd.Problem(2022, 11, Title = "Monkey in the Middle"):
  def run(input: String): Unit =
    val (monkeys, items) = prep(input).unzip
    part1(monkeys, items.map(_.clone))
    part2(monkeys, items.map(_.clone))
    ()

  type Item = Long
  case class Monkey(num: Int,
                    op: Item => Item,
                    testDivs: Int,
                    testTrue: Int,
                    testFalse: Int,
                    stressDown: Item => Item = _ / 3):
    var ops: Long = 0
    def doOp(wl: Item): (Int, Item) =
      ops = ops + 1
      val newWL = stressDown(op(wl))
      val thrown = if newWL % testDivs == 0 then testTrue else testFalse
      thrown -> newWL

  @tailrec
  def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)

  def lcm(a: Long, b: Long): Long = a / gcd(a, b) * b


  val rs: Regex = "Monkey (\\d+):\\n\\s+Starting items: (.*)\\n  Operation: new = old ([*+]) (\\d+|old)\\n  Test: divisible by (\\d+)\\n    If true: throw to monkey (\\d+)\\n    If false: throw to monkey (\\d+)\\n?".r

  def parse(s: String): (Monkey, mutable.Queue[Long]) =
    val rs(mn, sis, opo, opv, tss, its, ifs) = s: @unchecked
    val op = (opo, opv) match
      case ("*", "old") => (x: Long) => x * x
      case ("+", "old") => (x: Long) => x + x
      case ("*", v) => (x: Long) => x * v.toInt
      case ("+", v) => (x: Long) => x + v.toInt
    val tstD = tss.toInt
    val tstT = its.toInt
    val tstF = ifs.toInt
    new Monkey(mn.toInt, op, tstD, tstT, tstF) -> mutable.Queue.empty.addAll(sis.split(", ").map(_.toLong))

  def prep(input: String): Array[(Monkey, mutable.Queue[Item])] =
    time("\tprep", {
      input
        .splitByBlankLines
        .map(parse)
        .toArray
    })

  def doRound(monkeys: Array[Monkey], items: Array[mutable.Queue[Item]]): Array[mutable.Queue[Item]] =
    monkeys.foreach(m =>
      val iq = items(m.num)
      while iq.nonEmpty do
        val item = iq.dequeue
        val (idx, wl) = m.doOp(item)
        items(idx).enqueue(wl)
    )
    items

  def part1(monkeys: Array[Monkey], items: Array[mutable.Queue[Item]]): Long = part1 {
    (1 to 20).foldLeft(items)((is,_) => doRound(monkeys,is))
    monkeys.map(_.ops).sorted(Ordering.Long.reverse).take(2).product
  }

  def part2(monkeys: Array[Monkey], items: Array[mutable.Queue[Item]]): Long = part2 {
    val lcmDivs = monkeys.map(_.testDivs.toLong).reduce(lcm)
    val stressDown = (x: Long) => x % lcmDivs

    val newMonkeys = monkeys.map(_.copy(stressDown = stressDown))

    (1 to 10000).foldLeft(items)((is, _) => doRound(newMonkeys, is))
    newMonkeys.map(_.ops).sorted(Ordering.Long.reverse).take(2).product
  }