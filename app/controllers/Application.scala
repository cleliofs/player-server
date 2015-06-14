package controllers

import play.api.mvc._

import scala.annotation.tailrec

class Application extends Controller {

  def isSquare(n: Int) = {
    val square = Math.sqrt(n)
    square == square.asInstanceOf[Int]
  }
  def isCube(n: Int) = {
    val cube = Math.cbrt(n)
    cube == cube.asInstanceOf[Int]
  }


  def isPrime(n: Int) = {
    val divisors: Seq[Int] = for {
      i <- (1 to n) if n % i == 0
    } yield i

    if (divisors.size == 2 && divisors(1) == n) true
    else false
  }


  def fibSeq(max: Int): Seq[Int] = {
    @tailrec
    def rec(count: Int, acc: Seq[Int]): Seq[Int] = {
      if (count == max) acc
      else rec(count+1, acc ++ Seq(acc.takeRight(2).sum))
    }

    rec(1, Seq(1))
  }


  def index(q: String) = Action {
    println(q)

    val q1 = """[\w\s:]+ largest: ([\d\s,]+)""".r
    val q2 = """[\w\s:]+ ([\d]+) plus ([\d]+)""".r
    val q3 = """[\w\s:]+ ([\d]+) multiplied by ([\d]+)""".r
    val q4 = """[\w\s:]+ a square and a cube: ([\d\s,]+)""".r
    val q5 = """[\w\s:]+ are primes: ([\d\s,]+)""".r
    val q6 = """[\w]+: what colour is a ([\w]+)""".r
    val q7 = """[\w]+: what currency did ([\w]+) use before the Euro""".r
    val q8 = """[\w]+: who played James Bond in the film Dr No""".r
    val q9 = """[\w]+: which city is the Eiffel tower in""".r
    val q10 = """[\w]+: who is the Prime Minister of Great Britain""".r
    val q11 = """\w+: what is the (\d+)(?:st|th|nd) number in the Fibonacci sequence""".r
    val q12 = """[\w\s:]+ ([\d]+) minus ([\d]+)""".r

    var response = "Not answered!"

    if (q.matches(q1.toString())) {
      response = q1.findAllIn(q).matchData.map(m => m.group(1)).next().split(",").map(e => e.trim.toInt).max.toString
    } else if (q.matches(q2.toString())) {
      response = q2.findAllIn(q).matchData.map(m => m.group(1).toInt + m.group(2).toInt).next().toString
    } else if (q.matches(q3.toString)) {
      response = q3.findAllIn(q).matchData.map(m => m.group(1).toInt * m.group(2).toInt).next().toString
    } else if (q.matches(q4.toString)) {
      val elems = q4.findAllIn(q).matchData.map {
        m => m.group(1).trim().split(",")
      }.next().map(e => e.trim().toInt)

      val res = elems.filter(isSquare).filter(isCube)
      response = if (res.size > 0) res(0).toString else ""

    } else if (q.matches(q5.toString())) {
      val elems = q5.findAllIn(q).matchData.map {
        m => m.group(1).trim().split(",")
      }.next().map(e => e.trim().toInt)

      val res = elems.filter(isPrime)
      response = if (res.size > 0) res.mkString(", ") else ""
    } else if (q.matches(q6.toString())) {

      val word: String = q6.findAllIn(q).matchData.map {
        m => m.group(1)
      }.next()

      response = if (word == "banana") "yellow" else ""
    } else if (q.matches(q7.toString())) {
      val country: String = q7.findAllIn(q).matchData.map(m => m.group(1)).next()

      response = if (country == "Spain") "Peseta" else ""
    } else if (q.matches(q8.toString())) {
      response = "Sean Connery"
    } else if (q.matches(q9.toString())) {
      response = "Paris"
    } else if (q.matches(q10.toString())) {
      response = "David Cameron"
    } else if (q.matches(q11.toString())) {

      val elems = q11.findAllIn(q).matchData.map {
        m => m.group(1).trim().split(",")
      }.next().map(e => e.trim().toInt)

      response = fibSeq(elems(0)).last.toString
    } else if (q.matches(q12.toString())) {
      response = q12.findAllIn(q).matchData.map(m => m.group(1).toInt - m.group(2).toInt).next().toString
    }

    Ok(response)
  }

}
