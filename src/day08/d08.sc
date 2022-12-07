import math.{max,min}

val s = "30373\n25512\n65332\n33549\n35390"
.split("\n")
.map(_.map(_.asDigit).toArray)

val il = s.length-1
val jl = s.head.length-1

val fl = s.map(_.scanLeft(-1)(max))
val fr = s.map(_.scanRight(-1)(max))
val fu = s.transpose.map(_.scanLeft(-1)(max))
val fd = s.transpose.map(_.scanRight(-1)(max))

val vis = for
  i <- 0 to il
  j <- 0 to jl
  tree = s(i)(j)
  l = tree > fl(i)(j)
  r = tree > fr(i)(j+1)
  u = tree > fu(j)(i)
  d = tree > fd(j)(i+1)
  if l || r || d || u
yield 1

vis.sum
