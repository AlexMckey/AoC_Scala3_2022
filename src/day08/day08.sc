val s = "30373\n25512\n65332\n33549\n35390"
  .split("\n")
  .map(_.map(_.asDigit))

case class Vis(var l: Int, var r: Int, var u: Int, var d: Int):
  def check(h: Int) = h > l || h > r || h > u || h > d || h == 0
  def score: Int = l * r * u * d

import math.{max,min}

val il = s.length-1
val jl = s.head.length-1

val vis = Array.fill[Vis](5,5){Vis(0,0,0,0)}
for
  i <- 1 until il
  j <- 1 until jl
do
  vis(i)(j).l = max(vis(i)(j-1).l,s(i)(j-1))
  vis(i)(j).u = max(vis(i-1)(j).u,s(i-1)(j))
  vis(il-i)(jl-j).r = max(vis(il-i)(jl-j+1).r,s(il-i)(il-j+1))
  vis(il-i)(jl-j).d = max(vis(il-i+1)(jl-j).d,s(il-i+1)(jl-j))

vis(1)(1)
vis(2)(1)
vis(1)(2)
vis(2)(2)
vis(3)(2)
vis(3)(1)
vis(1)(3)
vis(3)(3)

val cnt = for
    i <- 0 to il
    j <- 0 to jl
yield vis(i)(j).check(s(i)(j))

vis(1)(3).check(s(1)(3))

il * 2 + jl * 2 + cnt.count(_ == true)

val dist = Array.fill[Vis](5,5){Vis(0,0,0,0)}
for
  i <- 1 until il
  j <- 1 until jl
do
  dist(i)(j).l = {
    var c = 1; while j-c > 0 && s(i)(j) > s(i)(j-c) do c += 1; c
  }
  dist(i)(j).r = {
    var c = 1; while j+c < il && s(i)(j) > s(i)(j+c) do c += 1; c
  }
  dist(i)(j).u = {
    var c = 1; while i-c > 0 && s(i)(j) > s(i-c)(j) do c += 1; c
  }
  dist(i)(j).d = {
    var c = 1; while i+c < jl && s(i)(j) > s(i+c)(j) do c += 1; c
  }

extension (n: Int)
  def orNotFound(x: Int): Int = if n < 0 then x else n

val a = Array(1,2,3,2,2,3,1,4,2,1)
val l = a.length - 1
var i = 2
var q = a(i)
a.indexWhere(_ >= q, i+1).orNotFound(l) - i
q = a.reverse(l-i)
a.reverse.indexWhere(_ >= q, l-i+1).orNotFound(l)-l+i
i = 7
q = a(i)
a.indexWhere(_ >= q, i+1).orNotFound(l) - i
q = a.reverse(l-i)
a.reverse.indexWhere(_ >= q, l-i+1).orNotFound(l) - l + i
i = 5
q = a(i)
a.indexWhere(_ >= q, i+1).orNotFound(l) - i
q = a.reverse(l-i)
a.reverse.indexWhere(_ >= q, l-i+1).orNotFound(l) - l + i

val score =
  for
    i <- 0 to il
    j <- 0 to jl
    tree = s(i)(j)
    l = j - s(i).lastIndexWhere(_ >= tree, j - 1).orNotFound(0)
    r = s(i).indexWhere(_ >= tree, j + 1).orNotFound(jl) - j
    u = i - s.lastIndexWhere(l => l(j) >= tree, i - 1).orNotFound(0)
    d = s.indexWhere(l => l(j) >= tree, i + 1).orNotFound(il) - i
  yield
    l * r * u * d

score.max
