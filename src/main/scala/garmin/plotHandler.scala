package garmin.plothandler

import plotly._, element._, layout._, Plotly._
import garmin.showSleepData._
import java.time

case class Canvas(dataSeq: Seq[(String, Int)], val xLabel: Axis, val yLabel: Axis, title: String="") {
  val trace = Scatter(
    dataSeq.map(_._1),
    dataSeq.map(_._2),
    name=time.LocalDate.parse(dataSeq.head._1).getYear.toString)

  val data = Seq(trace)
  def plot(): Unit = data.plot(title=title, xaxis=xLabel, yaxis=yLabel)
}
