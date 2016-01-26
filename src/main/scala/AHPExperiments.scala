/**
 * Created by Bondarchuk Yaroslav
 */

object AHPExperiments {
  def main (args: Array[String]): Unit = {

    val dataFile = "/home/yaroslav/Downloads/ahp/data/data.csv"

//    Максимальные значения для параметров, после которых мы их обрезаем
    val maxVals: Map[String, Long] =
      Map("Range" -> 4500, "MarketSize" -> 100,
        "Delivery" -> 999, "Buses/MS" -> 75,
        "WebClothes" -> 5000, "WebShoes" -> 1100)

//    Дополнительный знак для параметров. Если 1 - чем параметр больше - тем лучше, если -1 - чем меньше тем лучше.
    val extSigns: Map[String, Int] =
      Map("Range" -> -1, "MarketSize" -> 1,
        "Delivery" -> -1, "Buses/MS" -> 1,
        "WebClothes" -> 1, "WebShoes" -> 1)

// Коллекция с экспериментами
    val exp = for {
        i: String <- maxVals.map(_._1).toSet
        j: Double <- (1 to 5).map(_.toDouble/10)
    } yield {
      val preferencesFile = "/home/yaroslav/Downloads/ahp/data/preferences_11.txt"

      val (dataHeaders, data) = AHPCSVReader.readData(dataFile)
      val (prefHeaders, prefs) = AHPCSVReader.readPreferences(preferencesFile)

// функция добавляющая случайный равномерный шум в данные при анализе устойчивости.
// (с помощью зашумленных данных смотрим,
// как меняются результаты при неточной оценке значений параметров для городов)
      def addNoize(data: Dataset, paramName: String, noizeLevel: Double): Dataset = {
        def noize(value: Long) = value*(math.random*2-1)*noizeLevel

        Dataset(data.data.map(x =>
          Alternative(x.name,
            Params(
              x.params.params.map {
                p => if (p._1 == paramName)
                       (p._1, math.round(p._2 + noize(p._2)))
                     else (p._1, p._2)}))))
      }
// Функция, обрезающая данные по максимальным значениям
      def cutData(data: Dataset): Dataset = {
        def cut(value: Long , max: Long): Long = if (value > max) max else value
        Dataset(data.data.map(x =>
          Alternative(x.name,
            Params(x.params.params.map(x => (x._1, cut(x._2, maxVals(x._1))))))))
      }

//    Обрезанные данные
      val cData = cutData(addNoize(data, i, j))

//    Функция, вычисляющая разницу между максимальным и минимальным значением параметра в данных
      def maxDiffFunc(func: Params => Long): Long = {
        val max = cData.data.map(x => func(x.params)).max
        val min = cData.data.map(x => func(x.params)).min
        max - min
      }

//    Функция, расставляющая приоритеты городов по параметрам.
      def scoreFunction(alt1: Alternative, alt2: Alternative, getParamFunc: Params => Long, extSign: Int): Double = {
              val maxDiff = maxDiffFunc(getParamFunc)
              val sign = math.signum(getParamFunc(alt1.params) - getParamFunc(alt2.params))
              val d = math.abs(getParamFunc(alt1.params) - getParamFunc(alt2.params))
              math.pow(math.round(d.toDouble * 8 / maxDiff).toInt + 1, extSign*sign)
      }

//    Получить матрицу преференций
      def getPrefsMatrx(data: Dataset,
          func: (Alternative, Alternative, Params => Long, Int) => Double,
          getParamFunc: Params => Long, extSign: Int): Map[(String, String), Double] = {

        val alternatives = data.data
        val m = alternatives
          .map(alt1 =>
            alternatives
              .map(alt2 =>
                Map((alt1.name, alt2.name) -> func(alt1, alt2, getParamFunc, extSign)))
              .reduce(_ ++ _))
          .reduce(_ ++ _)
        m
      }

//    получить значения веса для пары (город, критерий)
      def getPropsPerfsMatrix: Map[(String, String), Double] = {
        val keys = prefs.prefs.flatMap(x => Set(x._1._1, x._1._2)).toSet
        val p = prefs.prefs ++
          prefs.prefs.map(x => ((x._1._2, x._1._1), 1.toDouble / x._2)) ++
          keys.toList.map(x => Map((x, x) -> 1.toDouble)).reduce(_ ++ _)
        p
      }

//    служебная функция, печатающая матрицу преференций
      def printPrefsMatrix(m: Map[(String, String), Double]): Unit = {
        val keys = m.keySet.map(_._1).toSeq

        keys.map(x => {
            val str = x + "\t" + keys
              .map(a =>  m.get(x, a).get.formatted("%f").substring(0, 4))
              .reduce(_ + "\t" + _)
            println(str)
          })
      }

//    Функция, получающая веса из матрицы преференций
      def getWeights(m: Map[(String, String), Double]) = {
        val keys = m.keySet.map(_._1).toList
        val norms = m.toList.map(x => (x._1._2, x._2)).groupBy(_._1).map(x => Map(x._1 -> x._2.map(_._2).sum)).reduce(_ ++ _)

        val normilizedMatrix = m.map(x => (x._1, x._2 / norms(x._1._2)))

        val weights = normilizedMatrix.toList.map(x => (x._1._1, x._2)).groupBy(_._1).map(x => Map(x._1 -> x._2.map(_._2).sum /
          keys.size)).reduce(_ ++ _)

        weights
      }


      val prefWeights = prefHeaders
        .map(h => Map(h -> getWeights(getPrefsMatrx(cData, scoreFunction, { x => x.params(h)}, extSigns(i)))))
        .reduce(_ ++ _)

      val propsW = getWeights(getPropsPerfsMatrix)

      println("")
      println("Prop with noize\t" + i + "\tlevel\t" + j)
      println("")
      println("preferences : ")
      prefs.prefs.toList.foreach(x => println(x._1._1 + "\t" + x._1._2 + "\t" + x._2.toString))
      println("")
      println("props weights : ")
      propsW.foreach(x => println(x._1 + "\t" + x._2))

      val commonWeights = prefWeights
        .toList
        .flatMap { case (prop, regionWeights) => regionWeights.toList.map { case (region, weight)  => (prop, region, weight)}}
        .map { case (prop, region, weight) => (region, prop, weight*propsW(prop))}


      val ranges = commonWeights.groupBy(_._1).toList
        .map { case (region, regionPropsWeights) =>
        (region,
          regionPropsWeights.map { case (r , prop, weight) => weight}.sum,
          regionPropsWeights.map { case (r , prop, weight) => prop + "\t"+ weight.toString}.reduce(_ + "\t" + _))}

      println("\n" + "Top 15:" + "\n")
      val r = ranges.sortBy(-_._2).take(15)

      r.foreach(x => println(x._1 + "\t" + x._2 + "\t" + x._3))
      r
    }
  }
}
