val s = "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
  .split("\n")
  .flatMap { s =>
    val s"$d $c" = s: @unchecked
    List.fill(c.toInt) {
      d
    }
  }

"R"*4

import AoC_Lib.Pos

def calNewTailPos(h: Pos, t: Pos): Pos =
  if h.near.contains(t)
  then t
  else
    val d = h - t
    t + (if d.x.abs > d.y.abs
      then Pos(d.x.sign * (d.x.abs - 1), d.y)
      else Pos(d.x, d.y.sign * (d.y.abs - 1)))

def move(cmd: String, h: Pos): Pos =
  h + (cmd match
    case "R" => Pos(1, 0)
    case "L" => Pos(-1, 0)
    case "U" => Pos(0, 1)
    case "D" => Pos(0, -1))
def calcPath_(rope: List[Pos], path: Array[String]): Set[Pos] =
  path.scanLeft(rope.head -> rope.last) { case ((h, t), dir) =>
    val newH = move(dir, h)
    newH -> calNewTailPos(newH, t)
  }.map(_._2).toSet

val r1 = List(Pos.zero, Pos.zero)
val r2 = List.fill(10){ Pos.zero}

calcPath_(r1, s).size

def calcPath(rope: List[Pos], path: Array[String]): Array[Pos] =
  path.scanLeft(rope){case (r, cmd) => r.tail.scanLeft(move(cmd,r.head))(calNewTailPos)}
    .map(_.last)


val ps1 = s.scanLeft(r1){case (r, cmd) => r.tail.scanLeft(move(cmd,r.head))(calNewTailPos)}
ps1.map(_.last).toSet.size

val ps2 = r2.tail.scanLeft(move("R", r2.head))(calNewTailPos)
val ps2_ = s.scanLeft(r2){case (r, cmd) => r.tail.scanLeft(move(cmd,r.head))(calNewTailPos)}
ps2_.map(_.last)

val s2 = "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"
  .split("\n")
  .flatMap { s =>
    val s"$d $c" = s: @unchecked
    List.fill(c.toInt) {
      d
    }
  }

val ps3_ = s2.scanLeft(r2){case (r, cmd) => r.tail.scanLeft(move(cmd,r.head))(calNewTailPos)}
ps3_.map(_.last).toSet.size

calcPath(r1,s).toSet.size
calcPath(r2,s).toSet.size
calcPath(r2,s2).toSet.size

calcPath(r2,s2)
