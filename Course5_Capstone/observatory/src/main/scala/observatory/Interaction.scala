package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math._

/**
  * 3rd milestone: interactive visualization
  */
object Interaction {

  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  /**
    * @param tile Tile coordinates
    * @return The latitude and longitude of the top-left corner of the tile, as per http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
    */
  def tileLocation(tile: Tile): Location = {
    new Location(tile.startLatDegree, tile.startLonDegree)
  }

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @param tile Tile coordinates
    * @return A 256×256 image showing the contents of the given tile
    */
  def tile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    Visualization.customVisualize(temperatures, colors,
      256, 256, 127,
      tile.startLatDegree, tile.endLatDegree,
      tile.startLonDegree, tile.endLonDegree)
  }

  def reducedTile(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)], tile: Tile): Image = {
    Visualization.customVisualize(temperatures, colors,
      128, 128, 127,
      tile.startLatDegree, tile.endLatDegree,
      tile.startLonDegree, tile.endLonDegree).scaleTo(256, 256)
  }

  /**
    * Generates all the tiles for zoom levels 0 to 3 (included), for all the given years.
    * @param yearlyData Sequence of (year, data), where `data` is some data associated with
    *                   `year`. The type of `data` can be anything.
    * @param generateImage Function that generates an image given a year, a zoom level, the x and
    *                      y coordinates of the tile and the data to build the image from
    */
  def generateTiles[Data](
    yearlyData: Iterable[(Year, Data)],
    generateImage: (Year, Tile, Data) => Unit
  ): Unit = {
    for {
      data <- yearlyData
      zoom <- 0 to 3
      x <- 0 to (pow(2, zoom).toInt - 1)
      y <- 0 to (pow(2, zoom).toInt - 1)
    } {
      generateImage(data._1, new Tile(x, y , zoom), data._2)
    }
  }
}
