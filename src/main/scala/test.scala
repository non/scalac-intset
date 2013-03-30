package test

import com.google.caliper.{Benchmark, Param, SimpleBenchmark, Runner}
import scala.reflect.ClassTag
import scala.util.Random._

object Util {
  def pos(n: Int): Int = if (n > 0) n else if (n >= -1) 1 else -(n + 1)
  def nextPosInt() = nextInt(Int.MaxValue) + 1
}

import Util._

object IntSetBenchmarks extends MyRunner(classOf[IntSetBenchmarks])

class IntSetBenchmarks extends MyBenchmark {
  @Param(Array("4", "5", "6", "8", "10", "12", "14"))
  var pow: Int = 0

  var data: Array[Int] = null
  var data2: Array[Int] = null

  var miguelSet: IntSetMiguel = null
  var posIntSet: PosIntSet = null
  var intSet: IntSet = null

  var j = 1


  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextPosInt).map(n => if(n == 0) n + 1 else n)
    data2 = init(n / 10)(nextPosInt).map(n => if(n == 0) n + 1 else n)

    miguelSet = new IntSetMiguel()
    posIntSet = PosIntSet.empty()
    intSet = IntSet.empty()

    var i = 0
    while (i < n) {
      val item = data(i)
      miguelSet += item
      posIntSet += item
      intSet += item
      i += 1
    }
  }

  def timeBuildMiguel(reps: Int) = run(reps) {
    val s = new IntSetMiguel()
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildPosIntSet(reps: Int) = run(reps) {
    val s = PosIntSet.empty()
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildIntSet(reps: Int) = run(reps) {
    val s = IntSet.empty()
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeContainsMiguel(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (miguelSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (miguelSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeContainsPosIntSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (posIntSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (posIntSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeContainsIntSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (intSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (intSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeDeleteMiguel(reps: Int) = run(reps) {
    val ms = miguelSet.copy
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { ms -= data(i); t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { ms -= data2(i); t += 1; i += 1 }
    t
  }

  def timeDeletePosIntSet(reps: Int) = run(reps) {
    val es = posIntSet.copy
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { es -= data(i); t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { es -= data2(i); t += 1; i += 1 }
    es.size
  }

  def timeDeleteIntSet(reps: Int) = run(reps) {
    val es = intSet.copy
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { es -= data(i); t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { es -= data2(i); t += 1; i += 1 }
    es.size
  }
}

/**
 * Objects extending Runner will inherit a Caliper-compatible main method.
 */
abstract class MyRunner(cls:java.lang.Class[_ <: Benchmark]) {
  def main(args:Array[String]): Unit = Runner.main(cls, args:_*)
}

/**
 * Extend Benchmark to gain some nice convenience methods.
 */
trait MyBenchmark extends SimpleBenchmark {
  // Sugar for building arrays using a per-cell init function.
  def init[A:ClassTag](size:Int)(init: => A) = {
    val data = Array.ofDim[A](size)
    for (i <- 0 until size) data(i) = init
    data
  }

  // Sugar to run 'f' for 'reps' number of times.
  def run(reps:Int)(f: => Unit) = for(i <- 0 until reps)(f)
}
