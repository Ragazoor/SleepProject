package garmin.showSleepData

import garmin.plothandler
import java.io.File
import java.io.PrintWriter
import java.time
import play.api.libs.json
import scala.io.Source
import plotly.element
import plotly.layout

trait Garmin {
  val defaultSleepTime = (7 * 60 * 60).toInt
  val wakeUpDate: time.LocalDate
  val totalSleep: Int
}

case class EmptyGarminObject(year: Int, dayOfYear: Int) extends Garmin {
  val totalSleep = defaultSleepTime
  val wakeUpDate = time.LocalDate.ofYearDay(year, dayOfYear)
}

case class GarminDataObject(jsonStruct: json.JsValue) extends Garmin{
  val wakeUpDate = time.LocalDate.parse((jsonStruct \\ "calendarDate").head.as[String])

  val sleepStartTime = time.LocalDateTime.parse((jsonStruct \\ "sleepStartTimestampGMT").head.as[String])
  val sleepEndTime = time.LocalDateTime.parse((jsonStruct \\ "sleepEndTimestampGMT").head.as[String])

  val confirmationType = (jsonStruct \\ "sleepWindowConfirmationType").head.as[String]
  val retro = (jsonStruct \\ "retro").head.as[Boolean]

  private def getSleepSeconds(tag: String): Int = {
    (jsonStruct \\ tag).headOption.getOrElse(json.JsNumber(0)).as[Int]
  }

  val deepSleepSeconds = getSleepSeconds("deepSleepSeconds")
  val lightSleepSeconds = getSleepSeconds("lightSleepSeconds")
  val remSleepSeconds = getSleepSeconds("remSleepSeconds")
  // val awakeSleepSeconds = getSleepSeconds("awakeSleepSeconds")
  // val unmeasurableSeconds = getSleepSeconds("unmeasurableSeconds")
  def getTotalSleep(): Int = {
    val totalSleep = lightSleepSeconds + deepSleepSeconds + remSleepSeconds
    if (totalSleep == 0) defaultSleepTime else totalSleep
  }
  val totalSleep = getTotalSleep()
}
// Factory for Day3 instances
object ShowSleepData {
  val inputDir= "processedData"
  val dataDir= "data"
  val secondsInOneMinute = 60

  def inputDirExists(): Boolean = {
    val inputDirObj = new File(inputDir)
    inputDirObj.exists || inputDirObj.mkdir
  }

  def preProcessFiles(dirObject: File): Unit = {
    val files = dirObject.listFiles.filter(_.isFile).toList
    files.foreach { file =>
      val splitString = Source.fromFile(file).mkString.split("[{]").mkString("{\"sleepDateData\": {")
      val processedString = splitString.split("[}]").mkString("}}")
      val newFileName = inputDir + "/" + file.getName.split("[.]").mkString("_processed.")

      if (inputDirExists) {
        val fileWriter = new PrintWriter(newFileName)
        fileWriter.write(processedString)
        fileWriter.close()
      }
    }
  }

  def parseInputFiles(dirObject: File): Vector[json.JsValue] = {
    val files = dirObject.listFiles.filter(_.isFile).toList
    files.flatMap { fileName =>
      val input = Source.fromFile(fileName).mkString
      val jsonValue: json.JsValue = json.Json.parse(input)
      (jsonValue \\ "sleepDateData")
    }.toVector
  }

  // def getBedtime(garminDataMap: Map[String, Seq[Garmin]]): Map[String, Seq[(String, Int)]] = {
  //   garminDataMap
  //   .map { case (key, garminSeq) =>
  //     val bedtimeSeq = 
  //     garminSeq
  //     .flatMap { garmin => 
  //       Seq(garmin.sleepStartTime.toLocalTime.toString, garmin.sleepEndTime.toLocalTime.toString)
  //     }
  //     val 
  //   }
  // }

  def getAccSleepPerYear(garminDataMap: Map[String, Seq[Garmin]]): Map[String, Seq[(String, Int)]] = {
    garminDataMap
    .map { case (key, garminSeq) =>
      val accSeq = garminSeq.scanLeft(0) {case (acc, garminObj) => acc + garminObj.totalSleep}
      val xyTuples = garminSeq.map(_.wakeUpDate.getDayOfYear.toString).zip(accSeq.map(_ / secondsInOneMinute*secondsInOneMinute))
      (key, xyTuples)
    }
  }

  def getAvgSleepPerMonth(garminDataMap: Map[String, Seq[Garmin]]): Map[String, Seq[(String, Int)]] = {

    def calculateAverageSeq(garminSeq: Seq[Garmin]) = {
      garminSeq
      .groupBy(_.wakeUpDate.getMonthValue.toString)
      .map { case (monthKey, garminSeq) => 
        (monthKey, garminSeq.foldLeft(0)(_+_.totalSleep) / garminSeq.length / secondsInOneMinute)
      }.toSeq
    }

    garminDataMap
    .map { case (yearKey, garmin) =>
      (yearKey, calculateAverageSeq(garmin))
    }
  }

  def getFilledInSequence(dayOfYearMap: Map[Int, Garmin], minX: Int, maxX: Int) = {
    val yearInt = 
      dayOfYearMap
      .headOption
      .map{ case (_, garminObj) => garminObj.wakeUpDate.getYear}.getOrElse(1986)

    (minX to maxX)
    .map { dayOfYear =>
      dayOfYearMap.get(dayOfYear) match {
        case Some(garmin) => garmin
        case None => EmptyGarminObject(yearInt, dayOfYear)
      }
    }.toSeq
  }

  // main method
  def main(args: Array[String]): Unit = {
    val dirFile = new File(dataDir)
    preProcessFiles(dirFile)
    val inputFile = new File(inputDir)
    val jsonVector = parseInputFiles(inputFile)

    val garminDataMap: Map[String, Seq[Garmin]] = 
      jsonVector.zipWithIndex.map {
        case (date, idx) => GarminDataObject(jsonVector(idx))
      }.groupBy(_.wakeUpDate.getYear.toString) // group the GarminObjects per year in a map
      .map { case (yearStr, garminSeq) => // Add default values to dates that has no values
        val dayOfYearMap = garminSeq.map(garminObj => (garminObj.wakeUpDate.getDayOfYear, garminObj)).toMap
        val minX = dayOfYearMap.map(_._1).min
        val maxX = dayOfYearMap.map(_._1).max
        // If some dayOfYear is missing between min and max, fill it in
        (yearStr, getFilledInSequence(dayOfYearMap, minX, maxX)) 
      }

    // Create canvas for accumulated sleep
    val accumulatedSleep = getAccSleepPerYear(garminDataMap)
    val xScatterLabel = layout.Axis(title="Day of year",
                                    showgrid=false,
                                    tickvals=Seq(0,31,59,90,120,151,181,212,243,273,304,334),
                                    ticktext=Seq("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
    val yScatterLabel = layout.Axis(title="Hours", showgrid=false)
    val accumulatedCanvas = plothandler.ScatterCanvas(accumulatedSleep, xScatterLabel, yScatterLabel, title="Cummulative sleep")

    //Create canvas for average sleep
    val averagedSleep = getAvgSleepPerMonth(garminDataMap)
    val xBarLabel = layout.Axis(title="Month",
                                showgrid=false,
                                tickvals=Seq(1,2,3,4,5,6,7,8,9,10,11,12),
                                ticktext=Seq("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

    val yBarLabel = layout.Axis(title="Hours",
                                showgrid=true,
                                tickvals=(1 to 8).map(_*60),
                                ticktext=(1 to 8).map(_.toString),
                                gridcolor=element.Color.StringColor("black"))
    val averagedCanvas = plothandler.BarCanvas(averagedSleep, xBarLabel, yBarLabel, title="Mean hours slept per night")

    //Create canvas for when I go to sleep
    // val bedtimeMap = getBedtime(garminDataMap)
    val xHistLabel = layout.Axis(title="Time of day",
                                showgrid=false)
                                // tickvals=Seq(1,2,3,4,5,6,7,8,9,10,11,12),
                                // ticktext=Seq("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))

    val yHistLabel = layout.Axis(title="Occurences",
                                showgrid=false)
                                // tickvals=(1 to 8).map(_*60),
                                // ticktext=(1 to 8).map(_.toString),
                                // gridcolor=element.Color.StringColor("black"))
    // val histCanvas = plothandler.HistogramCanvas(bedtimeMap, xHistLabel, yHistLabel, title="When I go to sleep")


    // accumulatedCanvas.plot(fileName="accumulatedSleep.html")
    // averagedCanvas.plot(fileName="averagedSleep.html")
    // histCanvas.plot(fileName="bedtime.html")
  }
}
