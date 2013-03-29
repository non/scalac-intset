package test

import com.google.caliper.{Benchmark, Param, SimpleBenchmark, Runner}

import scala.reflect.ClassTag

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

import scala.{specialized => spec}
import scala.collection.mutable
import scala.util.Random._

object IntSetBenchmarks extends MyRunner(classOf[IntSetBenchmarks])

class IntSetBenchmarks extends MyBenchmark {
  @Param(Array("4", "5", "6", "8", "10", "12", "14"))
  var pow: Int = 0

  var data: Array[Int] = null
  var data2: Array[Int] = null

  var miguelSet: IntSetMiguel = null
  var erikSet1: IntSetErik1 = null
  var erikSet2: IntSetErik2 = null

  var j = 1

  def nextPosInt = {
    val n = nextInt()
    if (n < 1) (n + 1) & 0x7fffffff else n
  }

  override protected def setUp() {
    val n = scala.math.pow(2, pow).toInt
    data = init(n)(nextPosInt).map(n => if(n == 0) n + 1 else n)
    data2 = init(n / 10)(nextPosInt).map(n => if(n == 0) n + 1 else n)

    miguelSet = new IntSetMiguel()
    erikSet1 = IntSetErik1.empty()
    erikSet2 = IntSetErik2.empty()

    var i = 0
    while (i < n) {
      val item = data(i)
      miguelSet += item
      erikSet1 += item
      erikSet2 += item
      i += 1
    }
  }

  def timeBuildMiguelSet(reps: Int) = run(reps)(buildMiguelSet)
  def timeBuildErikSet1(reps: Int) = run(reps)(buildErikSet1)
  def timeBuildErikSet2(reps: Int) = run(reps)(buildErikSet2)
  
  def timeContainsMiguelSet(reps: Int) = run(reps)(containsMiguelSet)
  def timeContainsErikSet1(reps: Int) = run(reps)(containsErikSet1)
  def timeContainsErikSet2(reps: Int) = run(reps)(containsErikSet2)

  def timeDeleteMiguelSet(reps: Int) = run(reps)(deleteMiguelSet)
  def timeDeleteErikSet1(reps: Int) = run(reps)(deleteErikSet1)
  def timeDeleteErikSet2(reps: Int) = run(reps)(deleteErikSet2)
  
  def buildMiguelSet: Int = {
    val s = new IntSetMiguel()
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def buildErikSet1: Int = {
    val s = IntSetErik1.empty()
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def buildErikSet2: Int = {
    val s = IntSetErik2.empty()
    var i = 0
    val len = data.length
    while (i < len) { s += data(i); i += 1 }
    s.size
  }

  def containsMiguelSet: Int = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (miguelSet(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (miguelSet(data2(i))) t += 1; i += 1 }
    t
  }

  def containsErikSet1: Int = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (erikSet1(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (erikSet1(data2(i))) t += 1; i += 1 }
    t
  }

  def containsErikSet2: Int = {
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { if (erikSet2(data(i))) t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { if (erikSet2(data2(i))) t += 1; i += 1 }
    t
  }

  def deleteMiguelSet: Int = {
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

  def deleteErikSet1: Int = {
    val es = erikSet1.copy
    var i = 0
    var len = data.length
    var t = 0
    while (i < len) { es -= data(i); t += 1; i += 1 }
    i = 0
    len = data2.length
    while (i < len) { es -= data2(i); t += 1; i += 1 }
    t
  }

  def deleteErikSet2: Int = {
    val es = erikSet2.copy
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


object IntSetErik1 {
  def empty() = new IntSetErik1(new Array[Int](8), 0, 0)
}

final class IntSetErik1 private[test] (as: Array[Int], n: Int, u: Int) extends Function1[Int, Boolean] {

  var items: Array[Int] = as
  var len: Int = n // how many elements are in the set
  var used: Int = u // how many slots are currently in use

  // hashing internals
  var mask = items.length - 1 // size-1, used for hashing
  var limit = (items.length * 0.65).toInt // point at which we should resize up

  final def size: Int = len

  @inline final def hash(item: Int, items0: Array[Int], mask0: Int): Int = {
    //var i = item & mask
    var i = scala.util.hashing.byteswap32(item) & mask
    while (true) {
      val found = items0(i)
      if (found == 0) {
        return i
      } else if (found == item) {
        return i | 0x80000000
      } else {
        i = (i + 1) & mask
      }
    }
    -1 //impossible
  }

  final def +=(item: Int): Boolean = {
    //var i = item & mask
    var i = scala.util.hashing.byteswap32(item) & mask
    while (true) {
      val j = i & mask
      val found = items(j)
      if (found <= 0) {
        items(j) = item
        len += 1
        if (found == 0) {
          used += 1
          if (used > limit) resize()
        }
        return true
      } else if (found == item) {
        return false
      } else {
        i = (i + 1) & mask
      }
    }
    false // impossible
  }

  final def -=(item: Int): Boolean = {
    val j = hash(item, items, mask)
    if (j < 0) {
      items(j & 0x7fffffff) = -1
      len -= 1
      true
    } else {
      false
    }
  }

  final def copy: IntSetErik1 = new IntSetErik1(items.clone, len, used)

  final def apply(item: Int): Boolean = hash(item, items, mask) < 0

  final def resize() {
    val size = items.length
    val factor = if (size < 10000) 4 else 2

    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextitems = new Array[Int](nextsize)

    var i = 0
    val lim = items.length - 1
    while (i < lim) {
      val item = items(i)
      if (item > 0) nextitems(hash(item, nextitems, nextmask)) = item
      i += 1
    }

    items = nextitems
    mask = nextmask
    limit *= factor
  }
}

object IntSetErik2 {
  def empty() = new IntSetErik2(new Array[Int](8), 0, 0)
}

final class IntSetErik2 private[test] (as: Array[Int], n: Int, u: Int) extends Function1[Int, Boolean] {

  var items: Array[Int] = as
  var len: Int = n // how many elements are in the set
  var used: Int = u // how many slots are currently in use

  // hashing internals
  var mask = items.length - 1 // size-1, used for hashing
  var limit = (items.length * 0.65).toInt // point at which we should resize up

  final def size: Int = len

  @inline final def hash(item: Int, items0: Array[Int], mask0: Int): Int = {
    var i = item & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask0
      val found = items0(j)
      if (found == 0) {
        return j
      } else if (found == item) {
        return j | 0x80000000
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    -1 //impossible
  }

  final def +=(item: Int): Boolean = {
    var i = item & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val found = items(j)
      if (found > 0) {
        if (found == item)
          return false
        else {
          i = (i << 2) + i + perturbation + 1
          perturbation = perturbation >> 5
        }
      } else {
        items(j) = item
        len += 1
        if (found == 0) {
          used += 1
          if (used > limit) resize()
        }
        return true
      }
    }
    false // impossible
  }

  final def -=(item: Int): Boolean = {
    val j = hash(item, items, mask)
    if (j < 0) {
      println("found item=%d at j=%s (%s)" format (item, j & 0x7fffffff, items(j & 0x7fffffff)))
      items(j & 0x7fffffff) = -1
      len -= 1
      true
    } else {
      false
    }
  }

  final def copy: IntSetErik2 = new IntSetErik2(items.clone, len, used)

  final def apply(item: Int): Boolean = hash(item, items, mask) < 0

  final def resize() {
    val size = items.length
    val factor = if (size < 10000) 4 else 2

    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextitems = new Array[Int](nextsize)

    var i = 0
    val lim = items.length - 1
    while (i < lim) {
      val item = items(i)
      if (item > 0) nextitems(hash(item, nextitems, nextmask)) = item
      i += 1
    }

    items = nextitems
    mask = nextmask
    limit *= factor
  }
}


final class IntSetMiguel {
  private[test] var keytable: Array[Int] = newEmptyArray(32) // 32 is initSize
  private[test] var sz = 0

  def copy = {
    val ms = new IntSetMiguel
    ms.keytable = keytable.clone
    ms.sz = sz
    ms
  }
  
  private def newEmptyArray(size: Int) = new Array[Int](size)

  def insert(k: Int) {
    checkResize()

    var pos  = index(k)
    val nil  = 0
    var curr = keytable(pos)

    while (curr != nil && curr != k) {
      pos = (pos + 1) % keytable.length
      curr = keytable(pos)
    }

    keytable(pos) = k
    val added = curr == nil
    if (added) sz += 1
    
}

private def checkResize() {
  val loadFactor = 450
    if (sz * 1000 / loadFactor > keytable.length) {
      val okeytable = keytable
      val ncapacity = keytable.length * 2
      keytable = newEmptyArray(ncapacity)
      sz = 0

      var pos = 0
      val nil = 0
      while (pos < okeytable.length) {
        val curr = okeytable(pos)
        if (curr != nil) {
          insert(curr)
        }
        pos += 1
      }
    }
  }

  private def before(i: Int, j: Int) = {
    val d = keytable.length >> 1
    if (i <= j) j - i < d
    else i - j > d
  }

  private def index(hc: Int): Int = {
    math.abs(scala.util.hashing.byteswap32(hc)) % keytable.length
  }

  def apply(k: Int): Boolean = {
    var pos = index(k)
    val nil = 0
    var curr = keytable(pos)

    while (curr != nil && curr != k) {
      pos = (pos + 1) % keytable.length
      curr = keytable(pos)
    }

    (curr != nil)
  }

  def +=(k: Int) {
    insert(k)
  }

  def -=(k: Int) {
    var pos  = index(k)
    val nil  = 0
    var curr = keytable(pos)

    while (curr != nil && curr != k) {
      pos = (pos + 1) % keytable.length
      curr = keytable(pos)
    }

    if (curr != nil) {

      var h0 = pos
      var h1 = (h0 + 1) % keytable.length
      while (keytable(h1) != nil) {
        val h2 = index(keytable(h1))
        if (h2 != h1 && before(h2, h0)) {
          keytable(h0) = keytable(h1)
          h0 = h1
        }
        h1 = (h1 + 1) % keytable.length
      }

      keytable(h0) = nil
      sz -= 1

    }
  }

  def clear() {
    keytable = newEmptyArray(32)  // 32 is initSize
    sz = 0
  }

  def size: Int = sz
}
