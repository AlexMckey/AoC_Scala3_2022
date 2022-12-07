val s = "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
val ss = s.split("\n").toSeq

val files = scala.collection.mutable.Map.empty[String, Int].withDefaultValue(0)
val dirs = scala.collection.mutable.Map.empty[String, Seq[String]].withDefaultValue(Seq.empty)

import scala.annotation.tailrec
@tailrec
def parseRec2(cmds: Seq[String], curDirs: Seq[String] = Seq.empty): Unit =
  def parseDirList(cmds: Seq[String], curDir: String): Seq[String] =
    val (dirContent, cmdsTail) = cmds.span(!_.startsWith("$"))
    val (ds, fs) = dirContent.partition(_.startsWith("dir"))
    files.addOne(curDir, fs.map(_.split(" ").head.toInt).sum)
    dirs.addOne(curDir -> ds.map(_.split(" ").tail.head))
    cmdsTail

  if cmds.isEmpty then ()
  else cmds.head match
    case s"$$ cd .." =>
      parseRec2(cmds.tail, curDirs.tail)
    case s"$$ ls" =>
      val newCmds = parseDirList(cmds.tail, curDirs.head)
      parseRec2(newCmds, curDirs)
    case s"$$ cd $n" =>
      parseRec2(cmds.tail, n +: curDirs)

parseRec2(ss)

files
dirs
dirs.flatMap((k,v) => v.map(d => d -> k))

def update(path: List[String], sizes: Map[String, Int], delta: Int): Map[String, Int] =
  sizes ++ path.map(p => p -> (sizes(p) + delta))

def parse(cmds: Seq[String]): Seq[Int] =
  val root = List("/")
  val (_, sizes) = cmds.foldLeft(root -> Map(root.head -> 0)) {
    case ((path, sizes), cmd) => cmd match
    case s"$$ ls" => path -> update(path, sizes, -sizes(path.head))
    case s"$$ cd .." => path.tail -> sizes
    case s"$$ cd /" => path.takeRight(1) -> sizes
    case s"$$ cd $name" => (name +: path) -> sizes.updated(name, 0)
    case s"dir $_" => path -> sizes
    case s"$size $_" => path -> update(path, sizes, size.toInt)
  }
  sizes.values.toSeq
  
parse(ss)