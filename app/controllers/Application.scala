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
      i <- 1 to n if n % i == 0
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


  def index(q: String) = {
    println(q)

    val q1 = """[\w\s:]+ largest: ([\d\s,]+)""".r
    val q2 = """[\w\s:]+ ([\d]+) plus ([\d]+)""".r
    val q3 = """[\w\s:]+ ([\d]+) multiplied by ([\d]+)""".r
    val q4 = """[\w\s:]+ a square and a cube: ([\d\s,]+)""".r
    val q5 = """[\w\s:]+ are primes: ([\d\s,]+)""".r
    val q6 = """[\w]+: what colour is a ([\w]+)""".r
    val q7 = """[\w]+: what currency did ([\w]+) use before the Euro""".r
    val q8 = """[\w\s:]+ played ([\w\s]+) in the film Dr No""".r
    val q9 = """[\w]+: which city is the ([\w\s]+) in""".r
    val q10 = """[\w]+: who is the ([\w\s]+) of ([\w\s]+)""".r
    val q11 = """\w+: what is the (\d+)(?:st|th|nd) number in the Fibonacci sequence""".r
    val q12 = """[\w\s:]+ ([\d]+) minus ([\d]+)""".r

    val response = q match {
      case q1(numbers) => numbers.split(",").map(_.trim().toInt).max
      case q2(num1, num2) => (num1.toInt + num2.toInt).toString
      case q3(num1, num2) => (num1.toInt * num2.toInt).toString
      case q4(numbers) => {
        val res: Array[Int] = numbers.split(",").map(_.trim().toInt).filter(isSquare).filter(isCube)
        if (res.length > 0) res(0).toString else ""
      }
      case q5(numbers) => {
        val res: Array[Int] = numbers.split(",").map(_.trim().toInt).filter(isPrime)
        if (res.length > 0) res.mkString(",").toString else ""
      }
      case q6(fruit) => if (fruit == "banana") "yellow" else ""
      case q7(country) => if (country.toLowerCase == "Spain".toLowerCase) "Peseta" else ""
      case q8(actorName) => s"$actorName played Sean Connery"
      case q9(monument) => if (monument.toLowerCase == "eiffel tower") "Paris" else ""
      case q10(role, countryName) => if (role.toLowerCase == "prime minister" && countryName.toLowerCase == "great britan") "David Cameron" else ""
      case q11(num) => fibSeq(num.toInt).last.toString
      case q12(num1, num2) => (num1.toInt - num2.toInt).toString
      case _ => "Not answered"
    }

    response

//    Ok(response)
  }

}

object Application extends App {
  val app = new Application
//  val q = "f8fed360: what is the largest: 1, 4, 0, 100, 50"
//  val q = "f8fed360: how much is 1 plus 10"
//  val q = "f8fed360: how much is 2 multiplied by 10"
//  val q = "f8fed360: what number is a square and a cube: 9, 64, 100, 120, 6472"
//  val q = "f8fed360: what number are primes: 9, 64, 1, 7, 11, 19"
//  val q = "f8fed360: what colour is a banana"
//  val q = "f8fed360: what currency did Spain use before the Euro"
//  val q = "f8fed360: who played James Bond in the film Dr No"
//  val q = "f8fed360: which city is the Eiffel Tower in"
//  val q = "f8fed360: who is the Prime Minister of Great Britan"
  val q = "f8fed360: what is the 14th number in the Fibonacci sequence"
//    val q = "f8fed360: how much is 15 minus 10"
  println(app.index(q))
}
