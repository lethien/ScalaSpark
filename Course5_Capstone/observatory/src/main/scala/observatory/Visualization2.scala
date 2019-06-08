package observatory

import com.sksamuel.scrimage.{Image, Pixel}
import math.{floor, ceil}

/**
  * 5th milestone: value-added information visualization
  */
object Visualization2 {

  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  /**
    * @param point (x, y) coordinates of a point in the grid cell
    * @param d00 Top-left value
    * @param d01 Bottom-left value
    * @param d10 Top-right value
    * @param d11 Bottom-right value
    * @return A guess of the value at (x, y) based on the four known values, using bilinear interpolation
    *         See https://en.wikipedia.org/wiki/Bilinear_interpolation#Unit_Square
    */
  def bilinearInterpolation(
    point: CellPoint,
    d00: Temperature,
    d01: Temperature,
    d10: Temperature,
    d11: Temperature
  ): Temperature = {
    d00 * (1 - point.x) * (1 - point.y) +
      d10 * point.x * (1 - point.y) +
      d01 * (1 - point.x) * point.y +
      d11 * point.x * point.y
  }

  /**
    * @param grid Grid to visualize
    * @param colors Color scale to use
    * @param tile Tile coordinates to visualize
    * @return The image of the tile at (x, y, zoom) showing the grid using the given color scale
    */
  def visualizeGrid(
    grid: GridLocation => Temperature,
    colors: Iterable[(Temperature, Color)],
    tile: Tile
  ): Image = {
    customVisualizeGrid(grid, colors, tile)
  }

  def customVisualizeGrid(grid: GridLocation => Temperature,
                          colors: Iterable[(Temperature, Color)],
                          tile: Tile,
                      width: Int = 256, height: Int = 256, pixelAlpha: Int = 127): Image = {
    val numTasks = 4
    val taskSize = math.ceil(height / numTasks).intValue()
    val taskRanges = (0 until numTasks).map(n => (n * taskSize, math.min(n * taskSize + taskSize, height)))
    val resultImage = Image(width, height)

    // Tile offset of this tile in the zoom+8 coordinate system
    // Each pixel is a sub-tile of tile (256 = 2^8)
    val x0 = math.pow(2.0, 8).toInt * tile.x
    val y0 = math.pow(2.0, 8).toInt * tile.y

    def calculatePixelColor(img: Image, fromRow: Int, toRow: Int) = {
      for {
        i <- fromRow until toRow // row - y
        j <- 0 until width // col - x
      } {
        val pixel = {
          val loc = new Tile(x0 + j, y0 + i, tile.zoom + 8)
          val left = loc.startLonDegree.floor.toInt
          val right = loc.startLonDegree.ceil.toInt
          val bottom = loc.startLatDegree.floor.toInt
          val top = loc.startLatDegree.ceil.toInt

          val cell = new CellPoint(loc.startLonDegree - left, loc.startLatDegree - bottom)

          val temperature = bilinearInterpolation(cell,
            grid(new GridLocation(bottom, left)), grid(new GridLocation(top, left)),
            grid(new GridLocation(bottom, right)), grid(new GridLocation(top, right)))
          val color = Visualization.interpolateColor(colors, temperature)
          Pixel.apply(color.red, color.green, color.blue, pixelAlpha)
        }
        img.setPixel(j, i, pixel)
      }
    }

    val tasks = taskRanges.map(range => task { calculatePixelColor(resultImage, range._1, range._2) })
    tasks.foreach(_.join())

    resultImage
  }
}
