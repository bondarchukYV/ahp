/**
 * Created by yaroslav on 17.01.16.
 */

object ahpTest {
  def main (args: Array[String]): Unit = {

    for (i <- (1 to 10).map(_.toString)) {
      val dataFile = "/home/yaroslav/Downloads/ahp/data/data.csv"
      val preferencesFile = "/home/yaroslav/Downloads/ahp/data/preferences_" + i + ".txt"

      val (dataHeaders, data) = CSVReader.readData(dataFile)
      val (prefHeaders, prefs) = CSVReader.readPreferences(preferencesFile)

//      if (dataHeaders.toSet == prefHeaders) println("Equal headers : ok")
//      else println("Equal headers : error")

      val maxPop = 6000
      val maxAvgIncome = 50000
      val maxDelivery = 999
      val maxBuses = 3000

      val maxPopDiff: Long = {
        val max = data.data.map(_.params.pop.get).filter(_ < maxPop).max
        val min = data.data.map(_.params.pop.get).min
        max - min
      }

      val maxAvgIncomeDiff: Long = {
        val max = data.data.map(_.params.income.get).filter(_ < maxAvgIncome).max
        val min = data.data.map(_.params.income.get).min
        max - min
      }

      val maxDeliveryDiff: Long = {
        val max = data.data.map(_.params.delivery.get).filter(_ < maxDelivery).max
        val min = data.data.map(_.params.delivery.get).min
        max - min
      }

      val maxBusesDiff: Long = {
        val max = data.data.map(_.params.buses.get).filter(_ < maxBuses).max
        val min = data.data.map(_.params.buses.get).min
        max - min
      }

      def popScoreFunction(alt1: Alternative, alt2: Alternative): Double = {
        if (alt1.params.pop.get > maxPop && alt2.params.pop.get > maxPop) 1
        else
          if (alt1.params.pop.get > maxPop && alt2.params.pop.get < maxPop) 9
          else
            if (alt1.params.pop.get < maxPop && alt2.params.pop.get > maxPop)
              1.toDouble / 9
            else {
              val sign = math.signum(alt1.params.pop.get - alt2.params.pop.get)
              val d = math.abs(alt1.params.pop.get - alt2.params.pop.get)
              math.pow(math.round(d.toDouble * 8 / maxPopDiff).toInt + 1, sign)
            }
      }

      def avgIncomeScoreFunction(alt1: Alternative, alt2: Alternative): Double = {
        if (alt1.params.income.get > maxAvgIncome &&
          alt2.params.income.get > maxAvgIncome) 1.toDouble
        else
          if (alt1.params.income.get > maxAvgIncome &&
            alt2.params.income.get < maxAvgIncome) 9.toDouble
          else
            if (alt1.params.income.get < maxAvgIncome &&
              alt2.params.income.get > maxAvgIncome) 1.toDouble / 9.toDouble
            else {
              val sign = math.signum(alt1.params.income.get - alt2.params.income.get)
              val d = math.abs(alt1.params.income.get - alt2.params.income.get)
              math.pow(math.round(d.toDouble * 8 / maxAvgIncomeDiff).toInt + 1, sign)
            }
      }

      def deliveryScoreFunction(alt1: Alternative, alt2: Alternative): Double = {
        if (alt1.params.delivery.get > maxDelivery &&
          alt2.params.delivery.get > maxDelivery) 1
        else
          if (alt1.params.delivery.get > maxDelivery &&
            alt2.params.delivery.get < maxDelivery) 1.toDouble / 9
          else
            if (alt1.params.delivery.get < maxDelivery &&
              alt2.params.delivery.get > maxDelivery) 9
            else {
              val sign = math.signum(alt1.params.delivery.get - alt2.params.delivery.get)
              val d = math.abs(alt1.params.delivery.get - alt2.params.delivery.get)
              math.pow(math.round(d.toDouble * 8 / maxDeliveryDiff).toInt + 1, -sign)
            }
      }

      def busesScoreFunction(alt1: Alternative, alt2: Alternative): Double = {
        if (alt1.params.buses.get > maxBuses &&
          alt2.params.buses.get > maxBuses) 1.toDouble
        else
          if (alt1.params.buses.get > maxBuses &&
            alt2.params.buses.get < maxBuses) 9.toDouble
          else
            if (alt1.params.buses.get < maxBuses &&
              alt2.params.buses.get > maxBuses) 1.toDouble / 9
            else {
              val sign = math.signum(alt1.params.buses.get - alt2.params.buses.get)
              val d = math.abs(alt1.params.buses.get - alt2.params.buses.get)
              math.pow(math.round(d.toDouble * 8 / maxBusesDiff).toInt + 1, sign)
            }
      }

      val alts = data.data.map(_.name).toList

//      def printFunc(headers: Iterable[String], func: (Alternative, Alternative) => Double): Unit = {
//        val head = headers.head
//        for (i <- headers.tail) {
//          println("          - [" + head + ", " + i + ", " +
//            func(data.data.filter(_.name == head).head, data.data.filter(_.name == i).head) + "]")
//        }
//        if (headers.tail.size > 1) printFunc(headers.tail, func)
//      }

      //    printFunc(alts, popScoreFunction)
      //    println()
      //    printFunc(alts, avgIncomeScoreFunction)
      //    println()
      //    printFunc(alts, deliveryScoreFunction)
      //    println()
      //    printFunc(alts, busesScoreFunction)
//      def printData(data: Dataset) = {
//        data.data.foreach { x => {
//          println("  " + x.name + ":")
//          println("    Population: " + x.params.pop.get.toLong)
//          println("    Average income: " + x.params.income.get.toLong)
//          println("    Delivery: " + x.params.delivery.get.toLong)
//          println("    Buses: " + x.params.buses.get.toLong)
//        }
//        }
//
//        println("data size : " + data.data.size)
//      }

      def getPrefsMatrx(data: Dataset,
        func: (Alternative, Alternative) => Double): Map[(String, String), Double] = {
        val alternatives = data.data
        //      println("alternatives : " + alternatives.size)
        val m = (for {i <- alternatives
                      j <- alternatives} yield Map((i.name, j.name) -> func(i, j))).reduce(_ ++ _)

        //      println("m size : " + m.size)
        m
      }

      def getPropsPerfsMatrix: Map[(String, String), Double] = {
        val keys = prefs.prefs.flatMap(x => Set(x._1._1, x._1._2)).toSet
        val p = prefs.prefs ++
          prefs.prefs.map(x => ((x._1._2, x._1._1), 1.toDouble / x._2)) ++
          keys.toList.map(x => Map((x, x) -> 1.toDouble)).reduce(_ ++ _)
        p
      }

      def printPrefsMatrix(m: Map[(String, String), Double]) = {
        val keys = m.keySet.map(_._1).toSeq
//        println("keys : " + keys.size)
//        println("\t" + keys.toSeq.reduce(_ + "\t" + _))

        for (i <- keys) {
          val str = i + "\t" +
            (for (j <- keys) yield m.get(i, j).get.formatted("%f")
              .substring(0, 4)).reduce(_ + "\t" + _)
          println(str)
        }
      }

      def getWeights(m: Map[(String, String), Double]) = {
        val keys = m.keySet.map(_._1).toList
        val norms = m.toList.map(x => (x._1._2, x._2)).groupBy(_._1).map(x => Map(x._1 -> x._2.map(_._2).sum)).reduce(_ ++ _)

//        norms.foreach(println)

        val normilizedMatrix = m.map(x => (x._1, x._2 / norms(x._1._2)))

        val weights = normilizedMatrix.toList.map(x => (x._1._1, x._2)).groupBy(_._1).map(x => Map(x._1 -> x._2.map(_._2).sum /
          keys.size)).reduce(_ ++ _)

        weights
      }
      //    printData(data)
      //    getPrefsMatrx(data, popScoreFunction)
      //    printPrefsMatrix(getPrefsMatrx(data, popScoreFunction))
      val popW = getWeights(getPrefsMatrx(data, popScoreFunction))
      val incomeW = getWeights(getPrefsMatrx(data, avgIncomeScoreFunction))
      val deliveryW = getWeights(getPrefsMatrx(data, deliveryScoreFunction))
      val busesW = getWeights(getPrefsMatrx(data, busesScoreFunction))

      val propsW = getWeights(getPropsPerfsMatrix)
//      println(popW.map(_._2).sum + " : " + popW)
//      println(incomeW.map(_._2).sum + " : " + incomeW)
//      println(deliveryW.map(_._2).sum + " : " + deliveryW)
//      println(busesW.map(_._2).sum + " : " + busesW)

      println("prefs : ")
      prefs.prefs.toList.foreach(x => println(x._1._1 + "\t" + x._1._2 + "\t" + x._2.toString))
      println()
      println("propsW : ")
      propsW.foreach(x => println(x._1 + "\t" + x._2))

      val commonWeights =
        popW.toList.map(x => (x._1, x._2 * propsW("Population"))) ++
          incomeW.toList.map(x => (x._1, x._2 * propsW("AverageIncome"))) ++
          deliveryW.toList.map(x => (x._1, x._2 * propsW("Delivery"))) ++
          busesW.toList.map(x => (x._1, x._2 * propsW("Buses")))

      val ranges = commonWeights.groupBy(_._1).toList.map(x => (x._1, x._2.map(_._2).sum))

      println("\n" + "Top 15:" + "\n")
      ranges.sortBy(-_._2).take(15).foreach(x => println(x._1 + "\t" + x._2))
    }
  }
}

object CSVReader {

  def readData(filePath: String):(Seq[String], Dataset) = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = bufferedSource.getLines

    val headers = lines.toSeq.head.split(",").toSeq.tail

    val data = Dataset(lines.toSeq.map { line => Alternative.parse(line)})

    data.data.size

//    println("headers : " + headers)
//    println("data num : " + data.data.size)

    bufferedSource.close

    (headers, data)
  }

  def readPreferences(filePath: String): (Set[String], Preferences) = {
    val bufferedSource = io.Source.fromFile(filePath)
    val lines = bufferedSource.getLines
    val data = Preferences(lines.toSeq
      .map { line => Preferences.parsePreference(line)}.reduce(_ ++ _))
    val headers = data.getHeaders
//    println("headers : " + headers)
//    println("data : " + data)

    data.prefs.size

    bufferedSource.close

    (headers, data)
  }
}