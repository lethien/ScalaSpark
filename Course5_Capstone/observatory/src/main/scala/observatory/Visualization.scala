package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import org.apache.spark.rdd.RDD

import scala.math.{Pi, abs, asin, cos, max, min, pow, sin, sqrt}
import scala.util.Random

/**
  * 2nd milestone: basic visualization
  */
object Visualization {

  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val distances = temperatures.par.map(row => (greatCircleDistance(row._1, location), row._2))

    val predictedTemp = distances.find(dist => dist._1 <= 1.0) match {
      case Some((_, temperature)) => temperature
      case _ => {
        val weighting = distances.
          map(distance => (inverseWeightedDistance(distance._1), distance._2)).
          map(weighting => (weighting._1 * weighting._2, weighting._1)).
          reduce((w1, w2) => (w1._1 + w2._1, w1._2 + w2._2))

        weighting._1 / weighting._2
      }
    }

    predictedTemp
  }

  def reducedPredictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val boundary = getDistanceBoundary(location)

    val closeBy = temperatures.filter(point => boundary(point._1))
    val distances = {if(closeBy.size == 0) Random.shuffle(temperatures).take(4000) else closeBy}.
      par.map(row => (greatCircleDistance(row._1, location), row._2))

    val predictedTemp = distances.find(dist => dist._1 <= 1.0) match {
      case Some((_, temperature)) => temperature
      case _ => {
        val weighting = distances.
          map(distance => (inverseWeightedDistance(distance._1), distance._2)).
          map(weighting => (weighting._1 * weighting._2, weighting._1)).
          reduce((w1, w2) => (w1._1 + w2._1, w1._2 + w2._2))

        weighting._1 / weighting._2
      }
    }

    predictedTemp
  }



  /**
    * http://janmatuschek.de/LatitudeLongitudeBoundingCoordinates
    * @param maxDistance the boundary distance (in Km), default with 4000 Km
    * @param radius sphere's radius (in Km), default with earth's radius
    */
  def getDistanceBoundary(center: Location, maxDistance: Double = 6000, radius: Double = 6371.0): Location => Boolean = {
    val maxCentralAngle = maxDistance / radius

    var latRadMin = center.latRadians - maxCentralAngle
    var latRadMax = center.latRadians + maxCentralAngle

    val deltaLon = asin(sin(maxCentralAngle) / cos(center.latRadians))
    var lonRadMin = center.lonRadians - deltaLon
    var lonRadMax = center.lonRadians + deltaLon

    if(latRadMin < -Pi/2 || latRadMax > Pi/2) {
      latRadMin = max(-Pi/2, latRadMin)
      latRadMax = min(Pi/2, latRadMax)
      lonRadMin = -Pi
      lonRadMax = Pi
    }

    def checkWithin(location: Location): Boolean = {
      val withinLat = location.latRadians >= latRadMin && location.latRadians <= latRadMax
      val withinLon = if(lonRadMin > lonRadMax) location.lonRadians > lonRadMin || location.lonRadians < lonRadMax
        else location.lonRadians > lonRadMin && location.lonRadians < lonRadMax

      withinLat && withinLon
    }

    checkWithin
  }

  /**
    * Shortest distance between two points on the surface of a sphere
    * https://en.wikipedia.org/wiki/Great-circle_distance
    * @param loc1
    * @param loc2
    * @param radius sphere's radius in km, default with earth's radius
    * @return
    */
  def greatCircleDistance(loc1: Location, loc2: Location, radius: Double = 6371.0) = {
    val centralAngle = if(loc1.lat == loc2.lat && loc1.lon == loc2.lon) 0.0 // Same location
    else if(loc1.lat == -loc2.lat && loc1.lon == -loc2.lon) Pi // Antipodes
    else 2 * asin(
      sqrt(
        pow(sin(abs(loc1.latRadians - loc2.latRadians) / 2), 2) +
        cos(loc1.latRadians) * cos(loc2.latRadians) * pow(sin(abs(loc1.lonRadians - loc2.lonRadians) / 2), 2)
      )
    )

    centralAngle * radius // The distance
  }

  /**
    * https://en.wikipedia.org/wiki/Inverse_distance_weighting
    * @param distance
    * @param power
    * @return
    */
  def inverseWeightedDistance(distance: Double, power: Int = 6): Double = {
    1.0 / pow(distance, power)
  }

  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val sortedPoints = points.toList.sortBy(_._1)
    val uppers = sortedPoints.filter(_._1 > value)
    val same = sortedPoints.filter(_._1 == value)
    val lowers = sortedPoints.filter(_._1 < value)
    if(same.size == 1) same.head._2
    else if(uppers.size == 0) sortedPoints.last._2
    else if(lowers.size == 0) sortedPoints.head._2
    else {
      val (point1, point2) = (lowers.last, uppers.head)
      point1._2 + (point2._2 - point1._2) * ((value - point1._1) / (point2._1 - point1._1))
    }
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    customVisualize(temperatures, colors)
  }

  def customVisualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)],
                      width: Int = 360, height: Int = 180, pixelAlpha: Int = 255,
                      startLatDeg: Double = 90, endLatDeg: Double = -90,
                      startLonDeg: Double = -180, endLonDeg: Double = 180): Image = {
    val numTasks = 4
    val taskSize = math.ceil(height / numTasks).intValue()
    val taskRanges = (0 until numTasks).map(n => (n * taskSize, math.min(n * taskSize + taskSize, height)))
    val resultImage = Image(width, height)

    def calculatePixelColor(img: Image, fromRow: Int, toRow: Int) = {
      for {
        i <- fromRow until toRow // row - y
        j <- 0 until width // col - x
      } {
        val pixel = {
          val correspondingLocation = new Location(startLatDeg - (startLatDeg - endLatDeg) * i / height, (endLonDeg - startLonDeg) * j / width + startLonDeg)
          val temperature = reducedPredictTemperature(temperatures, correspondingLocation)
          val color = interpolateColor(colors, temperature)
          Pixel.apply(color.red, color.green, color.blue, pixelAlpha)
        }
        resultImage.setPixel(j, i, pixel)
      }
    }

    val tasks = taskRanges.map(range => task { calculatePixelColor(resultImage, range._1, range._2) })
    tasks.foreach(_.join())

    resultImage
  }
}

