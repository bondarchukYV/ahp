/**
 * Created by yaroslav on 17.01.16.
 */
case class Preferences(prefs: Map[(String, String), Double]) {
  def getHeaders = prefs.keySet.flatMap(x => Set(x._1, x._2))
}

object Preferences {
  def parsePreference(str: String) = str.split(", ") match {
    case Array(head1, head2, value) =>
      if (value.contains("/")) Map((head1, head2) -> value.split("/")(0).toDouble/value.split("/")(1).toDouble)
      else Map((head1, head2) -> value.toDouble)
  }
}
