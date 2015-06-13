package controllers

import play.api._
import play.api.mvc._

class Application extends Controller {

  def index(q: String) = Action {
    val q1 = """[^,]+: which of the following numbers is the largest: ([\d\s,]+)""".r
    val q2 = """[^,]+: what is ([\d]+) plus ([\d]+)""".r
    val q3 = """[^,]+:  """.r

    var response = "Not Found!"
    if (q.matches(q1.toString())) {
      response = q1.findAllIn(q).matchData.map(m => m.group(1)).next.split(",").map(e => e.trim.toInt).max.toString
    }

    if (q.matches(q2.toString())) {
      response = q2.findAllIn(q).matchData.map(m => (m.group(1).toInt + m.group(2).toInt)).next.toString
    }

//    val response = q1.split(q)(1).split(",").map(n => n.trim.toInt).max.toString

//    val response: String = q match {
//      case q1(n1, n2) => Seq(n1.toInt, n2.toInt).max.toString
//      case q2(n1, n2) => (n1.toInt + n2.toInt).toString
//      case _ => "Not found"
//    }

    Ok(response)
  }

}
