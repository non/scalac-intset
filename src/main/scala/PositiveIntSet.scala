package test

import scala.collection.mutable.{Builder, GrowingBuilder, MapBuilder}
import scala.collection.generic.CanBuildFrom

object PositiveIntSet {
  def empty() = new PositiveIntSet(new Array[Int](8), 0, 0)

  def apply(ns: Int*) = {
    val set = empty
    ns.foreach { n =>
      if (n < 0) throw new IllegalArgumentException(n.toString)
      set += n
    }
    set
  }
  
  def newBuilder: Builder[Int, PositiveIntSet] =
    new Builder[Int, PositiveIntSet] {
      private var elems: PositiveIntSet = PositiveIntSet.empty

      override def sizeHint(size: Int) {
        var n = 8
        while (n < size) {
          n *= 2
          if (n < 0) throw new IllegalArgumentException(size.toString)
        }
        elems = new PositiveIntSet(new Array[Int](n), 0, 0)
      }
  
      def +=(n: Int): this.type = {
        if (n < 0) throw new IllegalArgumentException(n.toString)
        elems += n
        this
      }
  
      def clear(): Unit = elems = PositiveIntSet.empty
  
      def result: PositiveIntSet = elems
    }
  
  implicit def canBuildFrom: CanBuildFrom[PositiveIntSet, Int, PositiveIntSet] =
    new CanBuildFrom[PositiveIntSet, Int, PositiveIntSet] {
      def apply(from: PositiveIntSet) = newBuilder
      def apply() = newBuilder
    }
}

final class PositiveIntSet private[test] (as: Array[Int], n: Int, u: Int)
  extends Function1[Int, Boolean] with Iterable[Int] { self =>

  private var items: Array[Int] = as
  private var len: Int = n // how many elements are in the set
  private var used: Int = u // how many slots are currently in use

  // hashing internals
  private var mask = items.length - 1 // size-1, used for hashing
  private var limit = (items.length * 0.65).toInt // resize at this point

  final override def size: Int = len

  final def +=(item: Int): Boolean = {
    var i = item & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val found = items(j)
      if (found > 0) {
        if (found == item) {
          return false
        } else {
          i = (i << 2) + i + perturbation + 1
          perturbation = perturbation >> 5
        }
      } else {
        if (found < 0 && apply(item)) return false
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

  def -=(item: Int): Boolean = {
    var i = item & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val found = items(j)
      if (found == 0) {
        return false
      } else if (found == item) {
        items(j) = -1
        len -= 1
        return true
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    false // impossible
  }

  final def copy: PositiveIntSet = new PositiveIntSet(items.clone, len, used)

  def apply(item: Int): Boolean = {
    var i = item & 0x7fffffff
    var perturbation = i
    while (true) {
      val found = items(i & mask)
      if (found == 0) {
        return false
      } else if (found == item) {
        return true
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    false // impossible
  }

  private def hash(item: Int, items0: Array[Int], mask0: Int): Int = {
    var i = item & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask0
      if (items0(j) == 0) return j
      i = (i << 2) + i + perturbation + 1
      perturbation = perturbation >> 5
    }
    -1 // impossible
  }

  final private def resize() {
    val size = items.length
    val factor = if (size < 10000) 4 else 2

    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextitems = new Array[Int](nextsize)

    var i = 0
    val lim = items.length
    while (i < lim) {
      val item = items(i)
      if (item > 0) nextitems(hash(item, nextitems, nextmask)) = item
      i += 1
    }

    items = nextitems
    mask = nextmask
    limit *= factor
  }

  override def foreach[U](f: Int => U): Unit = {
    var i = 0
    while (i < items.length) {
      val item = items(i)
      if (item > 0) f(item)
      i += 1
    }
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
    var npos = 0
    var epos = 0
    val elen = self.len
    val narr = items.clone
  
    def hasNext: Boolean = epos < elen
  
    def next: Int = {
      var i = npos
      while (i < narr.length) {
        val item = narr(i)
        i += 1
        if (item > 0) {
          npos = i
          epos += 1
          return item
        }
      }
      throw new NoSuchElementException
    }
  }
}
