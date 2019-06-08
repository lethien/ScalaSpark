import java.util.concurrent.{ForkJoinPool, ForkJoinTask, ForkJoinWorkerThread, RecursiveTask}
import scala.util.DynamicVariable
import org.apache.spark.sql.SparkSession
import org.apache.spark.sql.types.{DoubleType, StructField, StructType}
import org.apache.spark.{SparkContext}

package object observatory {
  type Temperature = Double // °C, introduced in Week 1
  type Year = Int // Calendar year, introduced in Week 1

  // Spark context
  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .config("spark.master", "local")
      .config("spark.sql.shuffle.partitions", "100")
      .getOrCreate()
  val sc: SparkContext = spark.sparkContext

  // Dataframes template
  val joinedTemperatureFileColumns = List("year", "month", "day", "lat", "lon", "temperature")
  val averageTemperatureColumns = List("lat", "lon", "temperature")
  val joinedTemperatureDfSchema = StructType.apply(
    joinedTemperatureFileColumns.map(col => StructField(col, DoubleType, false))
  )
  val averageTemperatureDfSchema = StructType.apply(
    averageTemperatureColumns.map(col => StructField(col, DoubleType, false))
  )

  // Color list
  object TemperatureColorList {
    val white = Color(255, 255, 255)
    val red = Color(255, 0, 0)
    val yellow = Color(255, 255, 0)
    val cyan = Color(0, 255, 255)
    val blue = Color(0, 0, 255)
    val purple = Color(255, 0, 255)
    val darkblue = Color(33, 0, 107)
    val black = Color(0, 0, 0)

    val temperatureColorList = List((-60.0, black), (-50.0, darkblue), (-27.0, purple), (-15.0, blue),
      (0.0, cyan),
      (12.0, yellow), (32.0, red), (60.0, white))

    val deviationColorList = List((-7.0, blue), (-2.0, cyan),
      (0.0, white),
      (2.0, yellow), (4.0, red), (7.0, black))
  }

  // Parallel helpers
  val forkJoinPool = new ForkJoinPool
  val scheduler =
    new DynamicVariable[TaskScheduler](new DefaultTaskScheduler)
  def task[T](body: => T): ForkJoinTask[T] = {
    scheduler.value.schedule(body)
  }
}

package observatory {
  abstract class TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T]
    def parallel[A, B](taskA: => A, taskB: => B): (A, B) = {
      val right = task {
        taskB
      }
      val left = taskA
      (left, right.join())
    }
  }

  class DefaultTaskScheduler extends TaskScheduler {
    def schedule[T](body: => T): ForkJoinTask[T] = {
      val t = new RecursiveTask[T] {
        def compute = body
      }
      Thread.currentThread match {
        case wt: ForkJoinWorkerThread =>
          t.fork()
        case _ =>
          forkJoinPool.execute(t)
      }
      t
    }
  }
}