package test

import scala.collection.IterableLike
import scala.collection.mutable.{Builder, GrowingBuilder, MapBuilder}
import scala.collection.generic.CanBuildFrom

object IntSet {
  def empty() = new IntSet(new Array[Int](8), new Array[Byte](8), 0, 0)

  def apply(ns: Int*) = {
    val set = empty
    ns.foreach(set += _)
    set
  }
  
  def newBuilder: Builder[Int, IntSet] =
    new Builder[Int, IntSet] {
      private var elems: IntSet = IntSet.empty

      override def sizeHint(size: Int) {
        var n = 8
        while (n < size) {
          n *= 2
          if (n < 0) throw new IllegalArgumentException(size.toString)
        }
        elems = new IntSet(new Array[Int](n), new Array[Byte](n), 0, 0)
      }
  
      def +=(n: Int): this.type = {
        elems += n
        this
      }
  
      def clear(): Unit = elems = IntSet.empty
  
      def result: IntSet = elems
    }

  implicit def canBuildFrom: CanBuildFrom[IntSet, Int, IntSet] =
    new CanBuildFrom[IntSet, Int, IntSet] {
      def apply(from: IntSet) = newBuilder
      def apply() = newBuilder
    }
}

final class IntSet private[test](as: Array[Int], bs: Array[Byte], n: Int, u: Int)
  extends Function1[Int, Boolean] with Iterable[Int] with IterableLike[Int, IntSet] { self =>

  private var items: Array[Int] = as
  private var buckets: Array[Byte] = bs
  private var len: Int = n
  private var used: Int = u

  // hashing internals
  private var mask = items.length - 1 // size-1, used for hashing
  private var limit = (items.length * 0.65).toInt // when we should resize

  override def size: Int = len

  def +=(item: Int): Boolean = {
    var i = item & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 3) {
        if (items(j) == item) {
          return false
        } else {
          i = (i << 2) + i + perturbation + 1
          perturbation = perturbation >> 5
        }
      } else {
        if (status == 2 && apply(item)) return false
        items(j) = item
        buckets(j) = 3
        len += 1
        if (status == 0) {
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
      val status = buckets(j)
      if (status == 3 && items(j) == item) {
        buckets(j) = 2
        len -= 1
        return true
      } else if (status == 0) {
        return false
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    false // impossible
  }

  def copy: IntSet = new IntSet(items.clone, buckets.clone, len, used)

  def apply(item: Int): Boolean = {
    var i = item & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val status = buckets(j)
      if (status == 0) {
        return false
      } else if (status == 3 && items(j) == item) {
        return true
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    false // impossible
  }

  private def hash(item: Int, mask0: Int, items0: Array[Int], buckets0: Array[Byte]): Int = {
    var i = item & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask0
      if (buckets0(j) == 3 && items0(j) != item) {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      } else {
        return j
      }
    }
    -1 // impossible
  }

  private def resize() {
    val size = items.length
    val factor = if (size < 10000) 4 else 2
    
    val nextsize = size * factor
    val nextmask = nextsize - 1
    val nextitems = new Array[Int](nextsize)
    val nextbs = new Array[Byte](nextsize)

    var i = 0
    while (i < buckets.length) {
      if (buckets(i) == 3) {
        val item = items(i)
        val j = hash(item, nextmask, nextitems, nextbs)
        nextitems(j) = item
        nextbs(j) = 3
      }
      i += 1
    }

    items = nextitems
    buckets = nextbs
    mask = nextmask
    limit *= factor
  }

  override def foreach[U](f: Int => U) {
    var i = 0
    while (i < buckets.length) {
      if (buckets(i) == 3) f(items(i))
      i += 1
    }
  }

  def iterator: Iterator[Int] = new Iterator[Int] {
    var epos = 0
    val elen = self.len

    var xpos = 0
    val xitems = items.clone
    val xbuckets = buckets.clone
  
    def hasNext: Boolean = epos < elen
  
    def next: Int = {
      var i = xpos
      while (epos < elen) {
        val status = xbuckets(i)
        if (status == 3) {
          xpos = i + 1
          epos += 1
          return xitems(i)
        }
        i += 1
      }
      throw new NoSuchElementException
    }
  }

  override def newBuilder = IntSet.newBuilder
}
