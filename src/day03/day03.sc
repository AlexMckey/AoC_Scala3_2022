val s = "vJrwpWtwJgWrhcsFMMfFFhFp\njqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\nPmmdzqPrVvPwwTWBwg\nwMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\nttgJtRGJQctTZtZT\nCrZsJsPPZsGzwwsLwLmpwMDw"
val rs = s.split("\n")
val c = rs.map(q => q.grouped(q.length/2)/*q.splitAt(q.length/2)*/)
val u = c.map(_.toList.map(_.toSet))
val m = u.map{ _.reduce(_ intersect _) }
val h = m.map(_.head)
val w = h.map(e => if e.isLower then e - 'a' + 1 else e - 'A' + 27)
w.sum
val t = rs.grouped(3)
t.map(_.reduce(_ intersect _)).toList