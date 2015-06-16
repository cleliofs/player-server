import scala.annotation.tailrec

val q = "3561b300: what is the 10th number in the Fibonacci sequence"
val p1 = """[\w]+: what is the ([\d]+)(?:st|nd|th) number in the Fibonacci sequence""".r
p1.findAllIn(q).groupCount
q.matches(p1.toString())
Seq(1,2,3,4,5).takeRight(2)

//val elems = p.findAllIn(q).matchData.map {
//  m => m.group(1).trim().split(",")
//}.next().map(e => e.trim().toInt)
//fibSeq(elems(0)).last



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
  case q7(country) => if (country == "Spain") "Peseta" else ""
  case q8 => "Sean Connery"
  case q9 => "Paris"
  case q10 => "David Cameron"
  case q11(num) => fibSeq(num.toInt).last.toString
  case q12(num1, num2) => (num1.toInt - num2.toInt).toString
  case _ => "Not answered"
}