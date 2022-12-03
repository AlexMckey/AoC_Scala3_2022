package AoC_Lib

import scala.annotation.targetName

trait BoxPosOps[A <: BoxPosOps[A]] extends PosOps[A]:
  @targetName("LE")
  def <=(that: A): Boolean
  def min(that: A): A
  def max(that: A): A

trait PosOps[A <: PosOps[A]]:
  @targetName("Plus")
  def +(that: A): A
  @targetName("Times")
  def *(k: Int): A
  @targetName("TimesLeft")
  def *:(k: Int): A = this * k
  @targetName("UnaryMinus")
  def unary_- : A = -1 *: this
  @targetName("Minus")
  def -(that: A): A = this + (-that)

  def manhattanDistance(that: A): Int

trait PosFactory[A <: PosOps[A]]:
  val zero: A

case class Pos(x: Int, y: Int) extends BoxPosOps[Pos]:
  @targetName("Plus")
  override def +(that: Pos): Pos =
    Pos(x + that.x, y + that.y)

  @targetName("Times")
  override def *(k: Int): Pos =
    Pos(k * x, k * y)

  override def manhattanDistance(that: Pos): Int =
    (x - that.x).abs + (y - that.y).abs

  @targetName("LE")
  override def <=(that: Pos): Boolean =
    x <= that.x && y <= that.y

  override def min(that: Pos): Pos =
    Pos(x min that.x, y min that.y)

  override def max(that: Pos): Pos =
    Pos(x max that.x, y max that.y)

  override def toString: String = s"[$x,$y]"
  
  def near4: Seq[Pos] = Pos.axisOffsets.map(_ + this)
  def near: Seq[Pos] = Pos.allOffsets.map(_ + this)
  def all9: Seq[Pos] = Pos.all9Sorted.map(_ + this)

object Pos extends PosFactory[Pos]:
  override val zero: Pos = Pos(0, 0)
  extension (s: String)
    def toPos: Pos =
      val Array(x,y) = s.split(',').map(_.toInt)
      Pos(x,y)

  val axisOffsets: Seq[Pos] = Seq(Pos(0, 1), Pos(-1, 0), Pos(1, 0), Pos(0, -1))
  val diagonalOffsets: Seq[Pos] = Seq(Pos(-1, 1), Pos(1, 1), Pos(-1, -1), Pos(1, -1))
  val allOffsets: Seq[Pos] = axisOffsets ++ diagonalOffsets
  val all9Sorted: Seq[Pos] = Seq(Pos(-1, -1), Pos(0, -1), Pos(1, -1),
                                 Pos(-1,  0), Pos(0,  0), Pos(1,  0),
                                 Pos(-1,  1), Pos(0,  1), Pos(1,  1))