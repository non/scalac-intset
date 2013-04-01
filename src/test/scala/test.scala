package test

import scala.collection.mutable

import org.scalatest.matchers.ShouldMatchers
import org.scalatest._
import prop._

import org.scalacheck.Arbitrary._

trait TestLike[T <: TestableSet[U], U <: Iterable[Int]]
  extends PropSpec with ShouldMatchers with GeneratorDrivenPropertyChecks {

  def emptySet(): T

  property("Util.pos(x) > 0") {
    forAll { x: Int =>
      Util.pos(x) should be > (0)
    }
  }

  property("add a single element (+=)") {
    forAll { (n1: Int, n2: Int) =>
      val x = Util.pos(n1)
      val y = Util.pos(n2)

      val same = x == y
      val set = emptySet()

      set.size should be === 0
      set(x) should be === false
      set(x + 8) should be === false
      set(y) should be === false
      
      set += x
      set.size should be === 1
      set(x) should be === true
      set(x + 8) should be === false
      set(y) should be === same

      set += x
      set(x) should be === true
      set(x + 8) should be === false
      set.size should be === 1
      set(y) should be === same
      
      set -= x
      set(x) should be === false
      set(x + 8) should be === false
      set.size should be === 0
      set(y) should be === false

      set -= x
      set(x) should be === false
      set(x + 8) should be === false
      set.size should be === 0
      set(y) should be === false
    }
  }

  property("adding elements (+=)") {
    forAll { ns: Set[Int] =>
      val xs = ns.map(Util.pos)
      val set = emptySet()

      var n = 0
      xs.foreach { x =>
        set.size should be === n
        set(x) should be === false
        set += x
        set(x) should be === true
        n += 1
      }

      set.size should be === xs.size

      xs.foreach { x =>
        set += x
        set.size should be === xs.size
      }

      xs.foreach { x =>
        set(x) should be === true
      }
    }
  }

  property("removing elements (-=)") {
    forAll { ns: Set[Int] =>
      val xs = ns.map(Util.pos)
      val set = emptySet()

      xs.foreach(set += _)

      set.size should be === xs.size

      var n = xs.size
      xs.foreach { x =>
        set(x) should be === true
        set -= x
        n -= 1
        set(x) should be === false
        set.size should be === n
      }
    }
  }

  property("random += and -=") {
    forAll { (ns: List[Int], bs: List[Boolean]) =>
      val tpls = ns.map(Util.pos).zip(bs)
      val set = emptySet()
      val control = mutable.Set.empty[Int]

      tpls.foreach {
        case (x, true) => 
          set += x
          control += x
        case (x, false) =>
          set -= x
          control -= x
      }

      if (set.size != control.size) {
        println("tpls=%s" format tpls)
        println("set=%s" format set)
        println("control=%s" format control)
        println("")
        sys.error("death to videodrome")
      }
      set.size should be === control.size
      control.foreach { x =>
        set(x) should be === true
      }
    }
  }

  property("bulk add (++=)") {
    forAll { (xs0: List[Int], ys0: List[Int]) =>
      val xs = xs0.map(Util.pos)
      val ys = ys0.map(Util.pos)

      val a = emptySet()
      val b = emptySet()
      val c = emptySet()

      a ++= xs
      xs.foreach(b += _)
      a.elems should be === b.elems

      a ++= ys
      c ++= (xs ++ ys)
      a.elems should be === c.elems
    }
  }

  property("union (|)") {
    forAll { (xs0: Set[Int], ys0: Set[Int]) =>
      val xs = xs0.map(Util.pos)
      val ys = ys0.map(Util.pos)
      val zs = xs | ys

      val as = emptySet()
      xs.foreach(as += _)

      val bs = emptySet()
      ys.foreach(bs += _)

      val cs = emptySet()
      zs.foreach(cs += _)

      (as | bs.elems) should be === cs.elems
    }
  }

  property("intersection (&)") {
    forAll { (xs0: Set[Int], ys0: Set[Int]) =>
      val xs = xs0.map(Util.pos)
      val ys = ys0.map(Util.pos)
      val zs = xs & ys

      val as = emptySet()
      xs.foreach(as += _)

      val bs = emptySet()
      ys.foreach(bs += _)

      val cs = emptySet()
      zs.foreach(cs += _)

      (as & bs.elems) should be === cs.elems
    }
  }

  property("difference (--)") {
    forAll { (xs0: Set[Int], ys0: Set[Int]) =>
      val xs = xs0.map(Util.pos)
      val ys = ys0.map(Util.pos)
      val zs = xs -- ys

      val as = emptySet()
      xs.foreach(as += _)

      val bs = emptySet()
      ys.foreach(bs += _)

      val cs = emptySet()
      zs.foreach(cs += _)

      (as -- bs.elems) should be === cs.elems
    }
  }

  property("size") {
    forAll { (xs0: List[Int], ys0: List[Int]) =>
      val xs = xs0.map(Util.pos)
      val ys = ys0.map(Util.pos)

      val as = emptySet() ++ (xs ++ ys)
      val control = (xs ++ ys).toSet
      as.size should be === control.size
    }
  }
}

class PositiveIntSetTest extends TestLike[TestablePositiveIntSet, PositiveIntSet] {
  def emptySet() = TestableSet.fromPositiveIntSet(PositiveIntSet.empty)
}

class IntSetTest extends TestLike[TestableIntSet, IntSet] {
  def emptySet() = TestableSet.fromIntSet(IntSet.empty)
}

class AnyRefSetTest extends TestLike[TestableAnyRefSet, AnyRefSet[Int]] {
  def emptySet() = TestableSet.fromAnyRefSet(AnyRefSet.empty[Int])
}

class SpecializedSetTest extends TestLike[TestableSpecializedSet, SpecializedSet[Int]] {
  def emptySet() = TestableSet.fromSpecializedSet(SpecializedSet.empty[Int])
}
