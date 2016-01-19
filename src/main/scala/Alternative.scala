/**
 * Created by yaroslav on 17.01.16.
 */

case class Params(pop: Option[Long], income: Option[Long],
  delivery: Option[Long], buses: Option[Long]) {
}


object Params {
  def parse(str: String): Params = str.split(",") match {
    case Array(delivery, income, buses, pop) =>
      Params(Some(pop.toLong), Some(income.toLong),
        Some(delivery.toLong), Some(buses.toLong))
    case Array(pop, income, delivery) =>
      Params(Some(pop.toLong), Some(income.toLong), Some(delivery.toLong), None)

  }
}

case class Alternative(name: String, params: Params) {
}

object Alternative {
  def parse(str: String) = str.split(",") match {
    case Array(name, _*) => Alternative(name.toString, Params.parse(str.split(",").tail.reduce(_ + "," + _)))
  }
}
