package day07

import AoC_Lib.*
import Inputs.*

import javax.swing.border.TitledBorder

object Day07 extends aocd.Problem(2022, 7, Title = "No Space Left On Device"):
  def run(input: String): Unit =
    val things = prep(input)
    part1(things)
    part2(things)
    ()

  def prep(input: String): Seq[String] =
    time("\tprep", { input.toStrs })

  type Path = List[String]

  def parse(cmds: Seq[String]): Seq[Int] =
    val root = List("/")
    val (_, sizes) = cmds.foldLeft(root -> Map(root -> 0)) {
      case ((path, sizes), cmd) => cmd match
        case s"$$ ls" => path -> update(path, sizes, -sizes(path))
        case s"$$ cd .." => path.tail -> sizes
        case s"$$ cd /" => path.takeRight(1) -> sizes
        case s"$$ cd $name" => (name +: path) -> sizes.updated(name +: path, 0)
        case s"dir $_" => path -> sizes
        case s"$size $_" => path -> update(path, sizes, size.toInt)
    }
    sizes.values.toSeq

  def update(path: Path, sizes: Map[Path, Int], delta: Int): Map[Path, Int] =
    sizes ++ path.tails.toList.init.map(p => p -> (sizes(p) + delta))

  def part1(terminalOutput: Seq[String]): Int = part1 {
    parse(terminalOutput).filter(_ < 100000).sum
  }

  def part2(terminalOutput: Seq[String]): Int = part2 {
    val sizes = parse(terminalOutput)
    sizes.filter(_ >= 30000000 + sizes.max - 70000000).min
  }