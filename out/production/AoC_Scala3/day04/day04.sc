import AoC_Lib.Pos

val s = "2-4,6-8\n2-3,4-5\n5-7,7-9\n2-8,3-7\n6-6,4-6\n2-6,4-8"

val ps = s.split("\n")
  .map(_.split(",").flatMap(_.split("-"))
    .map(_.toInt))
    .map { case Array(a, b, c, d) => Pos(a, b) -> Pos(c, d) }

val p = s.split("\n")
  .map{ s =>
    val s"$a-$b,$c-$d" = s: @unchecked
    Pos(a.toInt, b.toInt) -> Pos(c.toInt, d.toInt)
  }

def fullContain(p1: Pos, p2: Pos) =
  (p1.x >= p2.x && p1.y <= p2.y) ||
    (p2.x >= p1.x && p2.y <= p1.y)

def overlap(p1: Pos, p2: Pos) =
  (p2.x >= p1.x && p2.x <= p1.y) ||
    (p2.y >= p1.x && p2.y <= p1.y) ||
    (p1.x >= p2.x && p1.x <= p2.y) ||
    (p1.y >= p2.x && p1.y <= p2.y)

ps.count { fullContain }
ps.count { overlap }