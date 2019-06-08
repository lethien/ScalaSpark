package observatory.benchtests

import observatory.Extraction
import org.scalameter.{Key, Measurer, Warmer, config}
import org.scalatest.FunSuite

trait ExtractionTemperatureBenchTest extends FunSuite {
  test("measure of get stations speed") {
    val time = config(
      Key.exec.maxWarmupRuns -> 1,

      Key.exec.benchRuns -> 5,

      Key.verbose -> true

    ) withWarmer {

      new Warmer.Default

    } withMeasurer {

      new Measurer.IgnoringGC

    } measure {
      Extraction.getStationMap("/stations.csv")
    }
    println(s"total time for get station = $time")
  }

  test("measure of location temperature speed") {
    val time = config(
      Key.exec.maxWarmupRuns -> 1,

      Key.exec.benchRuns -> 5,

      Key.verbose -> true

    ) withWarmer {

      new Warmer.Default

    } withMeasurer {

      new Measurer.IgnoringGC

    } measure {
      Extraction.locateTemperatures(2015,"/stations.csv","/2015.csv")
    }
    println(s"total time for locate temperature = $time")
  }
}

trait ExtractionAverageBenchTest extends FunSuite {
  def extract = new {
    val years = Extraction.locateTemperatures(2015,"/stations.csv","/2015.csv")
  }

  test("measure of yearly average speed") {
    val avgData = extract.years
    val time = config(
      Key.exec.maxWarmupRuns -> 5,

      Key.exec.benchRuns -> 5,

      Key.verbose -> true

    ) withWarmer {

      new Warmer.Default

    } withMeasurer {

      new Measurer.IgnoringGC

    } measure {
      Extraction.locationYearlyAverageRecords(avgData)
    }
    println(s"total time for yearly average = $time")
  }

  test("measure of spark yearly average speed") {
    val avgDataFrame = Extraction.sparkLocateTemperatures(Extraction.getStationMap("/stations.csv"), 2015,"/2015.csv")
    val time = config(
      Key.exec.maxWarmupRuns -> 5,

      Key.exec.benchRuns -> 5,

      Key.verbose -> true

    ) withWarmer {

      new Warmer.Default

    } withMeasurer {

      new Measurer.IgnoringGC

    } measure {
      Extraction.collectLocationYearlyAverageRecords(Extraction.sparkLocationYearlyAverageRecords(avgDataFrame))
    }
    println(s"total time for spark yearly average = $time")
  }
}
