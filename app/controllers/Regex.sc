import scala.annotation.tailrec

val q = "f8fed360: what is the 21st number in the Fibonacci sequence"
val p = """[\w]+: what is the (\d+)(?:st|nd|th) number in the Fibonacci sequence""".r
p.findAllIn(q).groupCount
q.matches(p.toString())
def fibSeq(max: Int): Seq[Int] = {
  @tailrec
  def rec(count: Int, acc: Seq[Int]): Seq[Int] = {
    if (count == max) acc
    else rec(count+1, acc ++ Seq(acc.takeRight(2).sum))
  }
  rec(1, Seq(1))
}
// rec(1, Seq(1)) => Seq(1) ++ Seq(1+1) => Seq(1,2)
// rec(2, Seq(1,2)) => Seq(1,2) ++ Seq(1+1) => Seq(1,2)
// rec(3, Seq(1,2)) => Seq(1) ++ Seq(1+1) => Seq(1,2)
Seq(1,2,3,4,5).takeRight(2)
//fibSeq(16).last

//val elems = p.findAllIn(q).matchData.map {
//  m => m.group(1).trim().split(",")
//}.next().map(e => e.trim().toInt)
//fibSeq(elems(0)).last
