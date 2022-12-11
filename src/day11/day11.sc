import scala.annotation.tailrec
import scala.collection.mutable
case class Monkey(num: Int, op: Long => Long, testDivs: Int, testTrue: Int, testFalse: Int, stressDown: Long => Long = _ / 3):
  var ops: Long = 0
  def doOp(wl: Long): (Int, Long) =
    ops = ops + 1
    val newWL = stressDown(op(wl))
    val thrown = if newWL % testDivs == 0 then testTrue else testFalse
    thrown -> newWL

val rs = "Monkey (\\d+):\\n\\s+Starting items: (.*)\\n  Operation: new = old ([*+]) (\\d+|old)\\n  Test: divisible by (\\d+)\\n    If true: throw to monkey (\\d+)\\n    If false: throw to monkey (\\d+)".r
def parse(s: String): (Monkey, mutable.Queue[Long]) =
  val rs(mn,sis,opo,opv,tss,its,ifs) = s: @unchecked
  val op = (opo, opv) match
    case ("*", "old") => (x: Long) => x * x
    case ("+", "old") => (x: Long) => x + x
    case ("*", v) => (x: Long) => x * v.toInt
    case ("+", v) => (x: Long) => x + v.toInt
  val tstD = tss.toInt
  val tstT = its.toInt
  val tstF = ifs.toInt
  new Monkey(mn.toInt, op, tstD, tstT, tstF) -> mutable.Queue.empty.addAll(sis.split(", ").map(_.toLong))

var mis = "Monkey 0:\n  Starting items: 79, 98\n  Operation: new = old * 19\n  Test: divisible by 23\n    If true: throw to monkey 2\n    If false: throw to monkey 3\n\nMonkey 1:\n  Starting items: 54, 65, 75, 74\n  Operation: new = old + 6\n  Test: divisible by 19\n    If true: throw to monkey 2\n    If false: throw to monkey 0\n\nMonkey 2:\n  Starting items: 79, 60, 97\n  Operation: new = old * old\n  Test: divisible by 13\n    If true: throw to monkey 1\n    If false: throw to monkey 3\n\nMonkey 3:\n  Starting items: 74\n  Operation: new = old + 3\n  Test: divisible by 17\n    If true: throw to monkey 0\n    If false: throw to monkey 1"
  .split("\n\n")
  .map(parse)

val (mnks, itms) = mis.unzip

val nitems1 = itms.map(_.clone)
val nitems2 = itms.map(_.clone)
def doRound(monkes: Array[Monkey], items: Array[mutable.Queue[Long]]) =
  monkes.foreach(m =>
    val iq = items(m.num)
    while iq.nonEmpty do
      val item = iq.dequeue
      val (idx, wl) = m.doOp(item)
      items(idx).enqueue(wl)
  )
  items
(1 to 20).foldLeft(itms)((items,_) => doRound(mnks,items))

mnks.map(_.ops).sorted(Ordering.Long.reverse).take(2).product

@tailrec
def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = a / gcd(a, b) * b

val lcmDivs = mnks.map(_.testDivs.toLong).reduce(lcm)
val stressDown = (x: Long) => x % lcmDivs

var nmonkeys = mnks.map(_.copy(stressDown = stressDown))
nitems1

(1 to 10000).foldLeft(nitems1)((items,_) => doRound(nmonkeys,items))

nmonkeys.map(_.ops).sorted(Ordering.Long.reverse).take(2).product

def doRound_(monkes: Array[Monkey], items: Array[scala.collection.immutable.Queue[Long]]) =
  monkes.foldLeft(items)((acc, m) =>
    val iq = acc(m.num)
    while iq.nonEmpty do
      val (item, newQ) = iq.dequeue
      val (idx, wl) = m.doOp(item)
      items(idx).enqueue(wl)
      items(m.num) = newQ
    items
  )


nmonkeys = mnks.map(_.copy(stressDown = stressDown))
nitems2

(1 to 10000).foldLeft(nitems2)((items,_) => doRound_(nmonkeys,items))

nmonkeys.map(_.ops).sorted(Ordering.Long.reverse).take(2).product