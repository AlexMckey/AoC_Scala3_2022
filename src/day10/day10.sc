//val ins = "noop\naddx 3\naddx -5"
val ins = "addx 15\naddx -11\naddx 6\naddx -3\naddx 5\naddx -1\naddx -8\naddx 13\naddx 4\nnoop\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx 5\naddx -1\naddx -35\naddx 1\naddx 24\naddx -19\naddx 1\naddx 16\naddx -11\nnoop\nnoop\naddx 21\naddx -15\nnoop\nnoop\naddx -3\naddx 9\naddx 1\naddx -3\naddx 8\naddx 1\naddx 5\nnoop\nnoop\nnoop\nnoop\nnoop\naddx -36\nnoop\naddx 1\naddx 7\nnoop\nnoop\nnoop\naddx 2\naddx 6\nnoop\nnoop\nnoop\nnoop\nnoop\naddx 1\nnoop\nnoop\naddx 7\naddx 1\nnoop\naddx -13\naddx 13\naddx 7\nnoop\naddx 1\naddx -33\nnoop\nnoop\nnoop\naddx 2\nnoop\nnoop\nnoop\naddx 8\nnoop\naddx -1\naddx 2\naddx 1\nnoop\naddx 17\naddx -9\naddx 1\naddx 1\naddx -3\naddx 11\nnoop\nnoop\naddx 1\nnoop\naddx 1\nnoop\nnoop\naddx -13\naddx -19\naddx 1\naddx 3\naddx 26\naddx -30\naddx 12\naddx -1\naddx 3\naddx 1\nnoop\nnoop\nnoop\naddx -9\naddx 18\naddx 1\naddx 2\nnoop\nnoop\naddx 9\nnoop\nnoop\nnoop\naddx -1\naddx 2\naddx -37\naddx 1\naddx 3\nnoop\naddx 15\naddx -21\naddx 22\naddx -6\naddx 1\nnoop\naddx 2\naddx 1\nnoop\naddx -10\nnoop\nnoop\naddx 20\naddx 1\naddx 2\naddx 2\naddx -6\naddx -11\nnoop\nnoop\nnoop"
val s = ins.split("\n")
val s"$op $x" = s.last: @unchecked

val cycs = s.foldLeft((0,1,List.empty[(Int, Int)])){
  case((c, r, acc), op) =>
  op match
    case s"noop" => (c+1, r, (c,r) :: acc)
    case s"addx $x" => (c+2, r + x.toInt, (c+1,r) :: (c,r) :: acc)
}._3.reverse

cycs.filter(_._1 > 200)

val cs = List(20,60,100,140,180,220)
cs.map(c => c * cycs(c-1)._2).sum

val sb = cycs.foldLeft(new StringBuilder(140)){
  case (sb, (c,r)) =>
    if r-1 to r+1 contains c % 40 then sb.append("#") else sb.append(".")
}
sb.toString().grouped(40).mkString("\n","\n","")


val cycles = s.scanLeft(0 -> 1){ case((cycle, reg), op) =>
  op match
    case s"noop" => (cycle+1) -> reg
    case s"addx $x" => (cycle+2) -> (reg+x.toInt)
}
val res = cs.map(fc => cycles.find(c => c._1 == fc-1 || c._1 == fc-2)).map(_.map(_._2).getOrElse(1))
cs.zip(res).map(_ * _).sum
cycles.filter(_._1 >= 200)

val points = s.scanLeft(0, 1, new StringBuilder(240)){ case((c, r, sb), op) =>
  op match
    case s"noop" =>
      val newc = c + 1
      val newr = r
      val x = c % 40
      if x >= r-1 && x <= r+1 then sb.append("#") else sb.append(".")
      (newc, newr, sb)
    case s"addx $d" =>
      val newc = c + 2
      val newr = r + d.toInt
      val x = c % 40
      if x >= r - 1 && x <= r + 1 then sb.append("#") else sb.append(".")
      if x + 1 >= r - 1 && x + 1 <= r + 1 then sb.append("#") else sb.append(".")
      (newc, newr, sb)
}

points.last._3.toString().grouped(40).mkString("\n","\n","")
