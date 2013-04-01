package test

import scala.util.Random._
import scala.collection.mutable
import java.io._
import Util._

trait TestableSet[U <: Iterable[Int]] {
  def elems: U

  def +=(item: Int): Boolean
  def -=(item: Int): Boolean
  def apply(item: Int): Boolean
  def size: Int

  def +(rhs: Int): U
  def ++(rhs: Traversable[Int]): U
  def ++=(rhs: Traversable[Int]): Unit

  def -(rhs: Int): U
  def --(rhs: Traversable[Int]): U
  def --=(rhs: Traversable[Int]): Unit

  def |(rhs: U): U
  def |=(rhs: U): Unit
  def &(rhs: U): U
  def &=(rhs: U): Unit

  override def toString = elems.toString
}

class TestablePositiveIntSet(ns: PositiveIntSet) extends TestableSet[PositiveIntSet] {
  val elems = ns.copy
  def +=(item: Int): Boolean = elems += item
  def -=(item: Int): Boolean = elems -= item
  def apply(item: Int): Boolean = elems(item)
  def size: Int = elems.size
  def +(rhs: Int) = elems + rhs
  def ++(rhs: Traversable[Int]) = elems ++ rhs
  def ++=(rhs: Traversable[Int]) = elems ++= rhs
  def -(rhs: Int) = elems - rhs
  def --(rhs: Traversable[Int]) = elems -- rhs
  def --=(rhs: Traversable[Int]) = elems --= rhs
  def |(rhs: PositiveIntSet) = elems | rhs
  def |=(rhs: PositiveIntSet) = elems |= rhs
  def &(rhs: PositiveIntSet) = elems & rhs
  def &=(rhs: PositiveIntSet) = elems &= rhs
}

class TestableIntSet(ns: IntSet) extends TestableSet[IntSet] {
  val elems = ns.copy
  def +=(item: Int): Boolean = elems += item
  def -=(item: Int): Boolean = elems -= item
  def apply(item: Int): Boolean = elems(item)
  def size: Int = elems.size
  override def toString = elems.toString
  def +(rhs: Int) = elems + rhs
  def ++(rhs: Traversable[Int]) = elems ++ rhs
  def ++=(rhs: Traversable[Int]) = elems ++= rhs
  def -(rhs: Int) = elems - rhs
  def --(rhs: Traversable[Int]) = elems -- rhs
  def --=(rhs: Traversable[Int]) = elems --= rhs
  def |(rhs: IntSet) = elems | rhs
  def |=(rhs: IntSet) = elems |= rhs
  def &(rhs: IntSet) = elems & rhs
  def &=(rhs: IntSet) = elems &= rhs
}

class TestableAnyRefSet(ns: AnyRefSet[Int]) extends TestableSet[AnyRefSet[Int]] {
  val elems = ns.copy
  def +=(item: Int): Boolean = elems += item
  def -=(item: Int): Boolean = elems -= item
  def apply(item: Int): Boolean = elems(item)
  def size: Int = elems.size
  override def toString = elems.toString
  def +(rhs: Int) = elems + rhs
  def ++(rhs: Traversable[Int]) = elems ++ rhs
  def ++=(rhs: Traversable[Int]) = elems ++= rhs
  def -(rhs: Int) = elems - rhs
  def --(rhs: Traversable[Int]) = elems -- rhs
  def --=(rhs: Traversable[Int]) = elems --= rhs
  def |(rhs: AnyRefSet[Int]) = elems | rhs
  def |=(rhs: AnyRefSet[Int]) = elems |= rhs
  def &(rhs: AnyRefSet[Int]) = elems & rhs
  def &=(rhs: AnyRefSet[Int]) = elems &= rhs
}

class TestableSpecializedSet(ns: SpecializedSet[Int]) extends TestableSet[SpecializedSet[Int]] {
  val elems = ns.copy
  def +=(item: Int): Boolean = elems += item
  def -=(item: Int): Boolean = elems -= item
  def apply(item: Int): Boolean = elems(item)
  def size: Int = elems.size
  override def toString = elems.toString
  def +(rhs: Int) = elems + rhs
  def ++(rhs: Traversable[Int]) = elems ++ rhs
  def ++=(rhs: Traversable[Int]) = elems ++= rhs
  def -(rhs: Int) = elems - rhs
  def --(rhs: Traversable[Int]) = elems -- rhs
  def --=(rhs: Traversable[Int]) = elems --= rhs
  def |(rhs: SpecializedSet[Int]) = elems | rhs
  def |=(rhs: SpecializedSet[Int]) = elems |= rhs
  def &(rhs: SpecializedSet[Int]) = elems & rhs
  def &=(rhs: SpecializedSet[Int]) = elems &= rhs
}

object TestableSet {
  def fromPositiveIntSet(ns: PositiveIntSet) =
    new TestablePositiveIntSet(ns)
  def fromIntSet(ns: IntSet) =
    new TestableIntSet(ns)
  def fromAnyRefSet(ns: AnyRefSet[Int]) =
    new TestableAnyRefSet(ns)
  def fromSpecializedSet(ns: SpecializedSet[Int]) =
    new TestableSpecializedSet(ns)
}


object RandTest {
  def run(makeSet: () => TestableSet[_]) {
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
    run(() => TestableSet.fromSpecializedSet(SpecializedSet.empty[Int]))
  }
}
