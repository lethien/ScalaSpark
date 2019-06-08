package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.sql._
import org.apache.spark.sql.functions._

import scala.io.Source

/**
  * 1st milestone: data extraction
  */
object Extraction {

  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationsMap = getStationMap(stationsFile)

    Source.fromFile(getFilePathString(temperaturesFile)).getLines().toSeq. // Read temperatures file into list of lines
      map(line => line.split(",", -1)). // split each line into array of fields
      filter(fields => !fields(4).equals("9999.9") && stationsMap.contains((fields(0), fields(1)))). // Filter out records with missing temperature
      par.map(fields => {
        val location = stationsMap((fields(0), fields(1)))
        (
          (LocalDate.of(year, fields(2).toInt, fields(3).toInt), // Year, Month, Day
            new Location(location._1.toDouble, location._2.toDouble),
            fahrenheitToCelsius(fields(4).toDouble)) // Temperature
          )
      }).seq
  }

  /**
    * Spark version of locateTemperatures method
    * @param stationsMap     Map of stations data
    * @param year             Year number
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A Dataframe containing triplets (date, location, temperature)
    */
  def sparkLocateTemperatures(stationsMap: Map[(String, String), (String, String)], year: Year, temperaturesFile: String): DataFrame = {
    val data = sc.textFile(getFilePathString(temperaturesFile)). // Read temperatures file into list of lines
      map(line => line.split(",", -1)). // split each line into array of fields
      filter(fields =>
      !fields(4).equals("9999.9") && // Filter out records with missing temperature
        stationsMap.contains(((fields(0), fields(1))))  // Filter out records associate with unknown stations
    ).
      map(fields => Array(
        year.toDouble, fields(2).toDouble, fields(3).toDouble, // Year, Month, Day
        stationsMap((fields(0), fields(1)))._1.toDouble, stationsMap((fields(0), fields(1)))._2.toDouble, // Lat, Lon
        fahrenheitToCelsius(fields(4).toDouble)) // Temperature
      ). // Add Year, get Location and convert temperature to celsius
      map(row => Row.fromSeq(row))

    spark.createDataFrame(data, joinedTemperatureDfSchema)
  }

  def getFilePathString(fileName: String): String = {
    Paths.get(getClass.getResource(fileName).toURI).toString
  }

  def getStationMap(stationsFile: String): Map[(String, String), (String, String)] = {
    val filePath = getFilePathString(stationsFile)
    Source.fromFile(filePath).getLines().toSeq.par.
      map(line => line.split(",", -1)). // split each line into array of fields
      filter(fields => !fields(2).isEmpty && !fields(3).isEmpty). // ignore stations with no coordination information
      map(fields => ((fields(0), fields(1)), (fields(2), fields(3)))).seq.toMap
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    records.
      groupBy(_._2).par.
      mapValues(data => {
        val dataReduced = data.map(_._3)
        dataReduced.sum / dataReduced.size
      }).seq.toSeq
  }

  def collectLocationYearlyAverageRecords(df: DataFrame): Iterable[(Location, Temperature)] = {
    df.
      collect().par.
      map(row => (
        new Location(row.getAs[Double](averageTemperatureColumns(0)), row.getAs[Double](averageTemperatureColumns(1))),
        row.getAs[Double](averageTemperatureColumns(2))
      )).seq
  }

  /**
    * Spark version of locationYearlyAverageRecords
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def sparkLocationYearlyAverageRecords(records: DataFrame): DataFrame = {
    records.
      groupBy(col(joinedTemperatureFileColumns(3)), col(joinedTemperatureFileColumns(4))).
      agg(
        avg(col(joinedTemperatureFileColumns(5))).as(joinedTemperatureFileColumns(5))
      )
  }

  /**
    *
    * @param f temperature in fahrenheit
    * @return temperature in celsius
    */
  def fahrenheitToCelsius(f: Double): Double = (f - 32) * 5 /9
}
