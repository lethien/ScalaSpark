package observatory


import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTest extends FunSuite with Checkers {
  test("Antipodes returns true for antipodes locations") {
    var loc1 = new Location( 48.6, 7.8)
    var loc2 = new Location( -48.6, 7.8)
    assert(Visualization.greatCircleDistance(loc1, loc2) === math.Pi * 6371.0, f"$loc1 must be antipode of $loc2")
    assert(Visualization.greatCircleDistance(loc2, loc1) === math.Pi * 6371.0, f"$loc2 must be antipode of $loc1")
  }


  test("Distance increases with different latitudes") {
    var loc1 = new Location( 0, 7.8)
    var loc2 = new Location( 20, 7.8)
    var loc3 = new Location( 40, 7.8)
    assert(Visualization.greatCircleDistance(loc1, loc2) < Visualization.greatCircleDistance(loc1, loc3))
  }

  test("Distance revolves around the world") {

    // same hemisphere
    var locNorth1 = new Location( 80, 60)
    var locNorth2 = new Location( 88, -170)
    var locInNorth = new Location( 87, 60)
    assert(Visualization.greatCircleDistance(locNorth1, locInNorth) > Visualization.greatCircleDistance(locNorth2, locInNorth) )

    // longitude loops at 180

    // same hemisphere
    var locEast = new Location( 2, -179)
    var locWest = new Location( 2, 175)
    var locinWest = new Location( 2, 178)
    assert(Visualization.greatCircleDistance(locWest, locinWest) > Visualization.greatCircleDistance(locEast, locinWest) )
  }
}
