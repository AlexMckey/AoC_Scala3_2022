package AoC_Lib

import scala.io.Source
import scala.util.Using

object Inputs:
  val DataDir = """d:\YandexDisk\DevsExercises\AdventOfCode\PuzzleInput\2022"""
  def input(day: Int): Iterator[String] = Using.Manager{ use => use(Source.fromFile(f"$DataDir/input$day%02d.txt", "utf-8")).getLines}.get
  def inputStr(day: Int): String = Using.Manager{ use => use(Source.fromFile(f"$DataDir/input$day%02d.txt", "utf-8")).mkString.trim}.get
  extension (s: String)
    def splitByBlankLines: Seq[String] = s.split("\n\n").toIndexedSeq
    def toStrs: Seq[String] = s.linesIterator.toIndexedSeq
    def toInts: Seq[Int] = s.toStrs.map(_.toInt)
    def words: Seq[String] = s.split(' ').toIndexedSeq