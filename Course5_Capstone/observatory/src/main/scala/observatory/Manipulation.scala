package observatory

import org.apache.spark.sql.DataFrame
import org.apache.spark.sql.functions._

import scala.collection.mutable

/**
  * 4th milestone: value-added information
  */
object Manipulation {

  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  /**
    * @param temperatures Known temperatures
    * @return A function that, given a latitude in [-89, 90] and a longitude in [-180, 179],
    *         returns the predicted temperature at this location
    */
  def makeGrid(temperatures: Iterable[(Location, Temperature)]): GridLocation => Temperature = {
    val gridHashMap: mutable.HashMap[GridLocation, Temperature] = new mutable.HashMap[GridLocation, Temperature]()

    val (startLat, endLat, startLon, endLon) = (-90, 90, -180, 180)

    for {
      i <- startLat to endLat // row - y
      j <- startLon to endLon // col - x
    } {
      gridHashMap.put(new GridLocation(i, j), Visualization.predictTemperature(temperatures, new Location(i, j)))
    }

    gridHashMap
  }

  /**
    * @param temperaturess Sequence of known temperatures over the years (each element of the collection
    *                      is a collection of pairs of location and temperature)
    * @return A function that, given a latitude and a longitude, returns the average temperature at this location
    */
  def average(temperaturess: Iterable[Iterable[(Location, Temperature)]]): GridLocation => Temperature = {
    val averagedTemp = temperaturess.flatten.
      groupBy(_._1).par.
      mapValues(values =>
        values.map(
          value => (value._2, 1)).
            reduce((r1, r2) => (r1._1 + r2._1, r1._2 + r2._2))
      ).
      mapValues(value => value._1 / value._2).
      seq

    makeGrid(averagedTemp)
  }

  def sparkAverage(temperaturess: Iterable[DataFrame]): GridLocation => Temperature = {
    val gridsAverage: Iterable[(Location, Temperature)] = temperaturess.
      reduce(_ union _).
      groupBy(col(averageTemperatureColumns(0)), col(averageTemperatureColumns(1))).
      agg(avg(averageTemperatureColumns(2)).as(averageTemperatureColumns(2))).
      collect().
      map(row => (
        new Location(row.getAs[Double](averageTemperatureColumns(0)), row.getAs[Double](averageTemperatureColumns(1))),
        row.getAs[Double](averageTemperatureColumns(2))
      ))

    makeGrid(gridsAverage)
  }

  /**
    * @param temperatures Known temperatures
    * @param normals A grid containing the “normal” temperatures
    * @return A grid containing the deviations compared to the normal temperatures
    */
  def deviation(temperatures: Iterable[(Location, Temperature)], normals: GridLocation => Temperature): GridLocation => Temperature = {
    val grid = makeGrid(temperatures)

    def getDeviation(gridLocation: GridLocation): Temperature = {
      grid(gridLocation) - normals(gridLocation)
    }

    getDeviation
  }


}

