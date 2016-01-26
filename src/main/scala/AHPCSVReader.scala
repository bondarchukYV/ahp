/**
 * Created by Bondarchuk Yaroslav
 */
object AHPCSVReader {

  def readData(filePath: String):(Seq[String], Dataset) = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = bufferedSource.getLines

    val headers = lines.toSeq.head.split(",").toSeq.tail

    val data = Dataset(lines.toSeq.map { line => Alternative.parse(headers, line)})

    data.data.size

    bufferedSource.close

    (headers, data)
  }

  def readPreferences(filePath: String): (Set[String], Preferences) = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = bufferedSource.getLines
    val data = Preferences(lines.toSeq
      .map { line => Preferences.parsePreference(line)}.reduce(_ ++ _))
    val headers = data.getHeaders

    data.prefs.size

    bufferedSource.close

    (headers, data)
  }
}
