package aocd

import requests._

private[aocd] class Api:

  def getData(year: Int, day: Int, token: String): Response = get(
    Api.mkInputUrl(year, day),
    cookies = Map(
      "session" -> new java.net.HttpCookie("session", token)
    ),
    headers = Map("User-Agent" -> Api.USER_AGENT)
  )


object Api extends Api:
  val USER_AGENT = "advent-of-code-data scala-v1"

  def mkUrl(year: Int, day: Int): String = s"https://adventofcode.com/$year/day/$day"
  def mkInputUrl(year: Int, day: Int): String = mkUrl(year, day) + "/input"

case class TokenNotFound() extends Exception(TokenNotFound.msg) {}

object TokenNotFound:
  val msg = "You must provide your Advent of Code session token as environment variable as AOC_SESSION_TOKEN or in ~/.aocd/token"
