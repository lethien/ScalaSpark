package observatory.benchtests

import observatory.{Extraction, Location, Visualization}
import org.scalameter.{Key, Measurer, Warmer, config}
import org.scalatest.FunSuite

trait VisualizeBenchTest extends FunSuite {
  def fixture = new {
    val values = Extraction.locationYearlyAverageRecords(Extraction.locateTemperatures(2015,"/stations.csv","/2015.csv"))
  }

  test("measure simplified temp prediction speed on iterable"){
    val temploc = fixture.values
    val time = config(
      Key.exec.maxWarmupRuns -> 5,

      Key.exec.benchRuns -> 10,

      Key.verbose -> true

    ) withWarmer {

      new Warmer.Default

    } withMeasurer {

      new Measurer.IgnoringGC

    } measure {
      Visualization.reducedPredictTemperature(temploc,Location(38,24))
    }
    println(s"total time for temperature prediction simplified = $time")
  }

  test("measure temp prediction speed on iterable"){
    val temploc = fixture.values
    val time = config(
      Key.exec.maxWarmupRuns -> 5,

      Key.exec.benchRuns -> 10,

      Key.verbose -> true

    ) withWarmer {

      new Warmer.Default

    } withMeasurer {

      new Measurer.IgnoringGC

    } measure {
      Visualization.predictTemperature(temploc,Location(38,24))
    }
    println(s"total time for temperature prediction = $time")
  }
}