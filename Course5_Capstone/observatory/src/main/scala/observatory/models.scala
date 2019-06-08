package observatory

import scala.math.{Pi, atan, sinh, pow}

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  require(lat >= -90 && lat <= 90, "Lat value must be in -90 to 90. Value: (" + lat + ", " + lon + ")")
  require(lon >= -180 && lon <= 180, "Lon value must be in -180 to 180. Value: (" + lat + ", " + lon + ")")
  def latRadians: Double = lat * math.Pi / 180
  def lonRadians: Double = lon * math.Pi / 180
}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int) {
  val num_tiles = pow(2.0, zoom)
  val startLatDegree = ((atan(sinh(Pi * (1.0 - 2.0 * y / num_tiles))) * 180.0 / Pi + 90) % 180.0) - 90
  val endLatDegree = startLatDegree - 170.1022 / num_tiles
  val startLonDegree = ((x / num_tiles) % 1.0) * 360.0 - 180.0
  val endLonDegree = startLonDegree + 360 / num_tiles
}

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int) {
  require(lat >= -90 && lat <= 90, "Grid Lat must be between -90 and 90. Actual: " + lat + ", " + lon)
  require(lon >= -180 && lon <= 180, "Grid Lon must be between -180 and 180. Actual: " + lat + ", " + lon)
  private val location = new Location(lat, lon)
  def getLocation() = location
}

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double) {
  require(x >= 0.0 && x <= 1.0, "Cell x must be between 0.0 and 1.0. Actual: " + x + ", " + y)
  require(y >= 0.0 && y <= 1.0, "Cell y must be between 0.0 and 1.0. Actual: " + x + ", " + y)
}

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int) {
  def + (other: Color) = new Color(
    (this.red + other.red) % 256,
    (this.green + other.green) % 256,
    (this.blue + other.blue) % 256
  )

  def - (other: Color) = new Color(
    (this.red - other.red) % 256,
    (this.green - other.green) % 256,
    (this.blue - other.blue) % 256
  )

  def * (scalar: Double) = new Color(
    math.round(this.red * scalar).toInt % 256,
    math.round(this.green * scalar).toInt % 256,
    math.round(this.blue * scalar).toInt % 256
  )

  def / (scalar: Double) = this.*(1 / scalar)
}

