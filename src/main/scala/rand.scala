package test

import scala.util.Random._
import scala.collection.mutable
import java.io._
import Util._

trait TestableSet {
  def +=(item: Int): Boolean
  def -=(item: Int): Boolean
  def apply(item: Int): Boolean
  def size: Int
}

object TestableSet {
  def fromPositiveIntSet(ns: PositiveIntSet) = new TestableSet {
    val elems = ns.copy
    def +=(item: Int): Boolean = elems += item
    def -=(item: Int): Boolean = elems -= item
    def apply(item: Int): Boolean = elems(item)
    def size: Int = elems.size
    override def toString = elems.toString
  }

  def fromIntSet(ns: IntSet) = new TestableSet {
    val elems = ns.copy
    def +=(item: Int): Boolean = elems += item
    def -=(item: Int): Boolean = elems -= item
    def apply(item: Int): Boolean = elems(item)
    def size: Int = elems.size
    override def toString = elems.toString
  }

  def fromAnyRefSet(ns: AnyRefSet[Int]) = new TestableSet {
    val elems = ns.copy
    def +=(item: Int): Boolean = elems += item
    def -=(item: Int): Boolean = elems -= item
    def apply(item: Int): Boolean = elems(item)
    def size: Int = elems.size
    override def toString = elems.toString
  }
}


object RandTest {
  def run(makeSet: () => TestableSet) {
    for (round <- 0 until 20) {
      val size = nextInt(15000) + 300
      val ns = new Array[Int](size)
      for (i <- 0 until size) ns(i) = nextPosInt()

      def dump() {
        val pw = new PrintWriter(new File("dump.out"))
        pw.println(ns.mkString("Array(", ", ", ")"))
        pw.close()
      }
  
      val set = makeSet()
      val control = mutable.Set.empty[Int]
      var i = 0
      while (i < size) {
        val n = ns(i)
        set += n
        control += n
        if (!set(n)) {
          dump()
          sys.error("add failed i=%s n=%s" format (ns, i, n))
        }
        i += 1
      }
  
      if (set.size != control.size) {
        dump()
        sys.error("set.size=%s != control.size=%s" format (set.size, control.size))
      }
  
      i = 0
      while (i < size) {
        val n = ns(i)
        set -= n
        if (set(n)) {
          dump()
          sys.error("remove failed i=%s n=%s" format (ns, i, n))
        }
        control -= n
        i += 1
      }
  
      if (set.size != 0) {
        dump()
        sys.error("set.size=%s != size=0" format (set.size, size))
      }
  
      println("size=%s is ok" format size)
    }
  }

  def main(args: Array[String]) {
    run(() => TestableSet.fromPositiveIntSet(PositiveIntSet.empty))
    run(() => TestableSet.fromIntSet(IntSet.empty))
    run(() => TestableSet.fromAnyRefSet(AnyRefSet.empty[Int]))
  }
}
