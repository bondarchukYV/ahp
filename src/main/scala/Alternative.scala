/**
 * Created by yaroslav on 17.01.16.
 */

case class Params(params: Map[String, Long]) {
}


object Params {
  def parse(headers: Seq[String], str: String) = {
    val p = str.split(",")

    Params((for {
        i <- (0 to headers.size-1)
      } yield Map(headers(i) -> p(i).toLong)).reduce(_ ++ _))
    }
//  def parse(str: String): Params = str.split(",") match {
//    case Array(delivery, income, buses, pop) =>
//      Params(Some(pop.toLong), Some(income.toLong),
//        Some(delivery.toLong), Some(buses.toLong))
//    case Array(pop, income, delivery) =>
//      Params(Some(pop.toLong), Some(income.toLong), Some(delivery.toLong), None)
//
//  }
}

case class Alternative(name: String, params: Params) {
}

object Alternative {
  def parse(headers: Seq[String], str: String) = str.split(",") match {
    case Array(name, _*) => Alternative(name.toString,
      Params.parse(headers, str.split(",").tail.reduce(_ + "," + _)))
  }
}
