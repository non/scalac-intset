package test

import com.google.caliper.{Benchmark, Param, SimpleBenchmark, Runner}
import scala.reflect.ClassTag
import scala.util.Random._
import scala.collection.mutable

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
  var positiveIntSet: PositiveIntSet = null
  var intSet: IntSet = null
  var anyRefSet: AnyRefSet[Int] = null
  var scalaSet: mutable.Set[Int] = null
  var specializedSet: SpecializedSet[Int] = null

  var j = 1


  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextPosInt).map(n => if(n == 0) n + 1 else n)
    data2 = init(n / 10)(nextPosInt).map(n => if(n == 0) n + 1 else n)

    miguelSet = new IntSetMiguel()
    positiveIntSet = PositiveIntSet.empty
    intSet = IntSet.empty
    anyRefSet = AnyRefSet.empty[Int]
    scalaSet = mutable.Set.empty[Int]
    specializedSet = SpecializedSet.empty[Int]

    var i = 0
    while (i < n) {
      val item = data(i)
      miguelSet += item
      positiveIntSet += item
      intSet += item
      anyRefSet += item
      scalaSet += item
      specializedSet += item
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

  def timeBuildPositiveIntSet(reps: Int) = run(reps) {
    val s = PositiveIntSet.empty
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildIntSet(reps: Int) = run(reps) {
    val s = IntSet.empty
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildAnyRefSet(reps: Int) = run(reps) {
    val s = AnyRefSet.empty[Int]
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildScalaSet(reps: Int) = run(reps) {
    val s = mutable.Set.empty[Int]
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildSpecializedSet(reps: Int) = run(reps) {
    val s = SpecializedSet.empty[Int]
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

  def timeContainsPositiveIntSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (positiveIntSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (positiveIntSet(data2(i))) t += 1; i += 1 }
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

  def timeContainsAnyRefSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (anyRefSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (anyRefSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeContainsScalaSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (scalaSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (scalaSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeContainsSpecializedSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (specializedSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (specializedSet(data2(i))) t += 1; i += 1 }
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

  def timeDeletePositiveIntSet(reps: Int) = run(reps) {
    val es = positiveIntSet.copy
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

  def timeDeleteAnyRefSet(reps: Int) = run(reps) {
    val es = anyRefSet.copy
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { es -= data(i); t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { es -= data2(i); t += 1; i += 1 }
    es.size
  }

  def timeDeleteScalaSet(reps: Int) = run(reps) {
    val es = scalaSet.clone
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { es -= data(i); t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { es -= data2(i); t += 1; i += 1 }
    es.size
  }

  def timeDeleteSpecializedSet(reps: Int) = run(reps) {
    val es = specializedSet.copy
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

object SymbolSetBenchmarks extends MyRunner(classOf[SymbolSetBenchmarks])

class SymbolSetBenchmarks extends MyBenchmark {
  @Param(Array("4", "5", "6", "8", "10", "12", "14"))
  var pow: Int = 0

  var data: Array[Symbol] = null
  var data2: Array[Symbol] = null

  var anyRefSet: AnyRefSet[Symbol] = null
  var scalaSet: mutable.Set[Symbol] = null
  var specializedSet: SpecializedSet[Symbol] = null
  var symbolSet: SymbolSet = null

  var j = 1

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextInt).map(n => Symbol(n.toString))
    data2 = init(n / 10)(nextInt).map(n => Symbol(n.toString))

    anyRefSet = AnyRefSet.empty[Symbol]
    scalaSet = mutable.Set.empty[Symbol]
    specializedSet = SpecializedSet.empty[Symbol]
    symbolSet= SymbolSet.empty

    var i = 0
    while (i < n) {
      val item = data(i)
      anyRefSet += item
      scalaSet += item
      specializedSet += item
      symbolSet += item
      i += 1
    }
  }

  def timeBuildAnyRefSet(reps: Int) = run(reps) {
    val s = AnyRefSet.empty[Symbol]
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildScalaSet(reps: Int) = run(reps) {
    val s = mutable.Set.empty[Symbol]
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildSpecializedSet(reps: Int) = run(reps) {
    val s = SpecializedSet.empty[Symbol]
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeBuildSymbolSet(reps: Int) = run(reps) {
    val s = SymbolSet.empty
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def timeContainsAnyRefSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (anyRefSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (anyRefSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeContainsScalaSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (scalaSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (scalaSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeContainsSpecializedSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (specializedSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (specializedSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeContainsSymbolSet(reps: Int) = run(reps) {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (symbolSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (symbolSet(data2(i))) t += 1; i += 1 }
    t
  }

  def timeDeleteAnyRefSet(reps: Int) = run(reps) {
    val es = anyRefSet.copy
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { es -= data(i); t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { es -= data2(i); t += 1; i += 1 }
    es.size
  }

  def timeDeleteScalaSet(reps: Int) = run(reps) {
    val es = scalaSet.clone
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { es -= data(i); t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { es -= data2(i); t += 1; i += 1 }
    es.size
  }

  def timeDeleteSpecializedSet(reps: Int) = run(reps) {
    val es = specializedSet.copy
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { es -= data(i); t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { es -= data2(i); t += 1; i += 1 }
    es.size
  }

  def timeDeleteSymbolSet(reps: Int) = run(reps) {
    val es = symbolSet.copy
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
