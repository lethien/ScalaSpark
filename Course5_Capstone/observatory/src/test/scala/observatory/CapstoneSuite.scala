package observatory

import observatory.benchtests.{ExtractionAverageBenchTest, ExtractionTemperatureBenchTest, VisualizeBenchTest}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CapstoneSuite
  extends ExtractionTemperatureBenchTest
  with ExtractionAverageBenchTest
