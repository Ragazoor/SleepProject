package garmin.showSleepData

import garmin.plothandler
import java.io.File
import java.time
import play.api.libs.json._
import scala.io.Source
import plotly.layout

case class GarminDataObject(jsonStruct: JsValue) {
  val wakeUpDate = (jsonStruct \\ "calendarDate").head.as[String]
  val wakeUpYear = time.LocalDate.parse(wakeUpDate.toString).getYear.toString

  val sleepStartTime = (jsonStruct \\ "sleepStartTimestampGMT").head.as[String]
  val sleepEndTime = (jsonStruct \\ "sleepEndTimestampGMT").head.as[String]

  // if ConfirmationType "UNCONFIRMED", NO SECONDS
  val confirmationType = (jsonStruct \\ "sleepWindowConfirmationType").head.as[String]
  val retro = (jsonStruct \\ "retro").head.as[Boolean]

  def getSleepSeconds(tag: String, condition: Boolean): Int = {
    if (condition) (jsonStruct \\ tag).head.as[Int] else 0
  }

  private val confirmationTypeIsOk = confirmationType != "UNCONFIRMED"
  val deepSleepSeconds = getSleepSeconds("deepSleepSeconds", confirmationTypeIsOk)
  val lightSleepSeconds = getSleepSeconds("lightSleepSeconds", confirmationTypeIsOk)
  val awakeSleepSeconds = getSleepSeconds("awakeSleepSeconds", confirmationTypeIsOk)
  val unmeasurableSeconds = getSleepSeconds("unmeasurableSeconds", confirmationTypeIsOk)
  val totalSleep = lightSleepSeconds + deepSleepSeconds
}
// Factory for Day3 instances
object showSleepData {

  def getAccumulatedSleep(garminDataSeq: Seq[GarminDataObject]): Seq[(String, Int)] = {
    val accumulatedSleep = garminDataSeq.scanLeft(0) {case (acc, garminObj) => acc + garminObj.totalSleep}
    garminDataSeq.map(_.wakeUpDate).zip(accumulatedSleep.map(_ / 3600))
  }

  def parseInputFiles(dirName: File): Vector[JsValue] = {
    val files = dirName.listFiles.filter(_.isFile).toList
    files.flatMap { fileName =>
      val input = Source.fromFile(fileName).mkString
      val jsonValue: JsValue = Json.parse(input)
      (jsonValue \\ "sleepDateData").toVector
    }.toVector
  }

  // main method
  def main(args: Array[String]): Unit = {
    val jsonVector = parseInputFiles()
    val garminDataSeq: Seq[GarminDataObject] = 
      jsonVector.zipWithIndex.map {
        case (date, idx) => GarminDataObject(jsonVector(idx))
      }.toSeq
    
    // Create canvas for accumulated sleep
    val accumulatedSleep = getAccumulatedSleep(garminDataSeq)
    val xLabel = layout.Axis(title="Date", showgrid=false)
    val yLabel = layout.Axis(title="Hours", showgrid=false)
    val accumulatedCanvas = plothandler.Canvas(accumulatedSleep, xLabel, yLabel, title="Cummulative sleep")
    // accumulatedCanvas.plot()
  }
}
