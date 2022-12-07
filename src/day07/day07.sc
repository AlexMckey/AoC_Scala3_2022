val s = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
  .split("\n")
  .toSeq

def parse(cmds: Seq[String], path: Seq[String] = Seq.empty, acc: Map[String, Int] = Map.empty.withDefaultValue(0)): Map[String, Int] =
  if cmds.isEmpty then acc
  else cmds.head match
    case s"$$ cd .." => parse(cmds.tail, path.tail, acc)
    case s"$$ cd $d" => parse(cmds.tail, d +: path, acc)
    case s"$$ ls" =>
      val (content, newCmds) = cmds.tail.span(!_.startsWith("$"))
      val files = content.filterNot(_.startsWith("dir"))
        .map{ case s"$size $_" => size.toInt }.sum
      parse(newCmds, path, acc ++ path.map(d => d -> (acc(d) + files)))

parse(s)

val p = List("asas", "asdasgfas", "/")
p.tails.toList.init

var acc: Map[String, Int] = Map.empty.withDefaultValue(0)
var path = List.empty[String]
var nc0 = s

val s"$$ cd $d0" = nc0.head: @unchecked
nc0.tail.head
path = d0 +: path
val (c1: Seq[String], nc1: Seq[String]) = nc0.drop(2).span(!_.startsWith("$"))
var f1 = c1.filterNot(_.startsWith("dir"))
  .map { case s"$size $_" => size.toInt }.sum
acc = acc ++ path.map(d => d -> (acc(d) + f1))

val s"$$ cd $d1" = nc1.head: @unchecked
nc1.tail.head
path = d1 +: path
val (c2: Seq[String], nc2: Seq[String]) = nc1.drop(2).span(!_.startsWith("$"))
var f2 = c2.filterNot(_.startsWith("dir"))
  .map { case s"$size $_" => size.toInt }.sum
acc = acc ++ path.map(d => d -> (acc(d) + f2))

val s"$$ cd $d2" = nc2.head: @unchecked
nc2.tail.head
path = d2 +: path

path.tails.toList.init.map(_.mkString("/"))

val (c3: Seq[String], nc3: Seq[String]) = nc2.drop(2).span(!_.startsWith("$"))
var f3 = c3.filterNot(_.startsWith("dir"))
  .map { case s"$size $_" => size.toInt }.sum
acc = acc ++ path.map(d => d -> (acc(d) + f3))

nc3.head
path = path.tail
nc3.head
path = path.tail

val s"$$ cd $d3" = nc3.drop(2).head: @unchecked
nc3.drop(2).tail.head
path = d3 +: path

val (c4: Seq[String], nc4: Seq[String]) = nc3.drop(2).drop(2).span(!_.startsWith("$"))
var f4 = c4.filterNot(_.startsWith("dir"))
  .map { case s"$size $_" => size.toInt }.sum
acc = acc ++ path.map(d => d -> (acc(d) + f4))

