package garmin.plothandler

import plotly._, element._, layout._, Plotly._
import garmin.showSleepData._
import java.time

trait Canvas {
  val title: String
  val xLabel: Axis
  val yLabel: Axis
  val data: Seq[Trace]

  def plot(fileName: String): Unit = data.plot(title=title, xaxis=xLabel, yaxis=yLabel, path=fileName)
}

case class ScatterCanvas(dataMap: Map[String, Seq[(String, Int)]],
                         val xLabel: Axis,
                         val yLabel: Axis,
                         val title: String="") extends Canvas {

  val data = dataMap.map { case (key, data) =>
    Scatter(data.map(_._1), data.map(_._2), name=key)
  }.toSeq
}

case class BarCanvas(dataMap: Map[String, Seq[(String, Int)]],
                     val xLabel: Axis,
                     val yLabel: Axis,
                     val title: String="") extends Canvas {

  val data = dataMap.map { case (key, monthSeq) =>
    Bar(monthSeq.map(_._1), monthSeq.map(_._2), name=key)
  }.toSeq
}

// case class HistogramCanvas(dataMap: Map[String, Seq[(String, Int)]],
//                            val xLabel: Axis,
//                            val yLabel: Axis, 
//                            val title: String="") extends Canvas {
//   val data = dataMap.map { case (key, yearlyData) =>
//     Bar(monthSeq.map(_._1), monthSeq.map(_._2), name=key)
//   }.toSeq
// }