val win: Map[String, String] = Map("A" -> "C", "B" -> "A", "C" -> "B")
win("A")
val swin = win.map(_.swap)
swin("A")
val s = "A X"
val s"$a $b" = s: @unchecked