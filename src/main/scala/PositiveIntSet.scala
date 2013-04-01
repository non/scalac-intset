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

  private[test] var items: Array[Int] = as
  private[test] var len: Int = n // how many elements are in the set
  private[test] var used: Int = u // how many slots are currently in use

  // hashing internals
  private[test] var mask = items.length - 1 // size-1, used for hashing
  private[test] var limit = (items.length * 0.65).toInt // resize at this point

  final override def size: Int = len

  final def +(item: Int): PositiveIntSet = {
    val set = this.copy
    set += item
    set
  }

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

  def ++(rhs: Traversable[Int]): PositiveIntSet = {
    val set = this.copy
    rhs.foreach(set += _)
    set
  }

  def ++=(rhs: Traversable[Int]): Unit = rhs.foreach(this += _)

  final def -(item: Int): PositiveIntSet = {
    val set = this.copy
    set -= item
    set
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

  def --(rhs: Traversable[Int]): PositiveIntSet = {
    val set = this.copy
    rhs.foreach(set -= _)
    set
  }

  def --=(rhs: Traversable[Int]): Unit = rhs.foreach(this -= _)

  def |(rhs: PositiveIntSet): PositiveIntSet =
    if (size >= rhs.size) {
      val set = this.copy
      rhs.foreach(set += _)
      set
    } else {
      val set = rhs.copy
      this.foreach(set += _)
      set
    }

  def |=(rhs: PositiveIntSet): Unit = rhs.foreach(this += _)

  def &(rhs: PositiveIntSet): PositiveIntSet = {
    val set = PositiveIntSet.empty
    if (size <= rhs.size)
      this.foreach(n => if (rhs(n)) set += n)
    else
      rhs.foreach(n => if (this(n)) set += n)
    set
  }

  def &=(rhs: PositiveIntSet): Unit = {
    val set = this & rhs
    items = set.items
    len = set.len
    used = set.used
    mask = set.mask
    limit = set.limit
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

  override def equals(rhs: Any) = rhs match {
    case set: PositiveIntSet =>
      size == set.size && forall(set.apply)
    case _ =>
      false
  }
}
