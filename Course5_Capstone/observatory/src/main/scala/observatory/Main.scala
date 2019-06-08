package observatory

import java.io.File
import java.util.Calendar

import org.apache.spark.sql.DataFrame
import org.apache.spark.storage.StorageLevel

object Main extends App {

  def main() = {
    val formatter = new java.text.SimpleDateFormat("YYYY-MM-dd_HH-mm-ss")
    val startTime = Calendar.getInstance().getTime()
    println("Program start! " + formatter.format(startTime))

    // Extract data from csv files
    val (fromYear, toYear) = (1975, 2015) // (1975, 2015)

    val temperaturesOverYears: Seq[(Int, DataFrame)] = {
      for (i <- fromYear to toYear)
        yield (
          i,
          Extraction.sparkLocateTemperatures(Extraction.getStationMap("/stations.csv"), i, "/" + i + ".csv").persist(StorageLevel.MEMORY_AND_DISK_SER)
        )
    }

    // Get averaged temperature over the years of each location
    val averagedTemperaturesOverYear: Seq[(Int, DataFrame)] = temperaturesOverYears.
      map(yearRecords => (
        yearRecords._1,
        Extraction.sparkLocationYearlyAverageRecords(yearRecords._2).persist(StorageLevel.MEMORY_AND_DISK_SER)
      ))
    println("Extracting Dfs prepared! " + formatter.format(Calendar.getInstance().getTime()))

    // Generate Temperature Tiles
    for{
      record <- averagedTemperaturesOverYear
    } {
      val data = Extraction.collectLocationYearlyAverageRecords(record._2)
      println("Extracted data for " + record._1 + " at " + formatter.format(Calendar.getInstance().getTime()))
      val averageGrid = Manipulation.makeGrid(data)
      record._2.unpersist()
      println("Prepared for " + record._1 + " at " + formatter.format(Calendar.getInstance().getTime()))
      for {
        zoom <- 0 to 3
        y <- 0 to (math.pow(2, zoom).toInt - 1)
        x <- 0 to (math.pow(2, zoom).toInt - 1)
      } {
        val path = Extraction.getFilePathString("") + "/temperatures/" + record._1 + "/" + zoom + "/"
        val fileName = x + "-" + y + ".png"

        val folder = new File(path)
        if(!folder.exists()) folder.mkdirs()

        val image = Visualization2.visualizeGrid(averageGrid, TemperatureColorList.temperatureColorList, new Tile(x, y, zoom)).
          output(new File(path, fileName))
        println("Generated for " + path + fileName + " at " + formatter.format(Calendar.getInstance().getTime()))
      }
    }

    // Generate Deviation Tiles
    val baseData = temperaturesOverYears.filter(record => record._1 >= 1975 && record._1 <= 1989).map(_._2)
    val baseGrid = Manipulation.sparkAverage(baseData)
    println("Prepared base grid at " + formatter.format(Calendar.getInstance().getTime()))
    for{
      record <- averagedTemperaturesOverYear.filter(record => record._1 >= 1990 && record._1 <= 2015)
    } {
      val data = Extraction.collectLocationYearlyAverageRecords(record._2)
      println("Extracted data for " + record._1 + " at " + formatter.format(Calendar.getInstance().getTime()))
      val deviationGrid = Manipulation.deviation(data, baseGrid)
      record._2.unpersist()
      println("Prepared for " + record._1 + " at " + formatter.format(Calendar.getInstance().getTime()))
      for {
        zoom <- 0 to 3
        y <- 0 to (math.pow(2, zoom).toInt - 1)
        x <- 0 to (math.pow(2, zoom).toInt - 1)
      } {
        val path = Extraction.getFilePathString("") + "/deviations/" + record._1 + "/" + zoom + "/"
        val fileName = x + "-" + y + ".png"

        val folder = new File(path)
        if(!folder.exists()) folder.mkdirs()

        val image = Visualization2.visualizeGrid(deviationGrid, TemperatureColorList.deviationColorList, new Tile(x, y, zoom)).
          output(new File(path, fileName))
        println("Generated " + path + fileName + " at " + formatter.format(Calendar.getInstance().getTime()))
      }
    }

    // Done
    println("Done! Ran from " + formatter.format(startTime))
  }

  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  main()
}
