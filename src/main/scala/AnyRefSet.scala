package test

import scala.collection.IterableLike
import scala.collection.mutable.{Builder, GrowingBuilder, MapBuilder}
import scala.collection.generic.CanBuildFrom

import scala.reflect.ClassTag

object AnyRefSet {
  def empty[A]() = new AnyRefSet[A](new Array[AnyRef](8), new Array[Byte](8), 0, 0)

  def apply[A](ns: A*) = {
    val set = empty[A]
    ns.foreach(set += _)
    set
  }
  
  def newBuilder[A]: Builder[A, AnyRefSet[A]] =
    new Builder[A, AnyRefSet[A]] {
      private var elems: AnyRefSet[A] = AnyRefSet.empty[A]

      override def sizeHint(size: Int) {
        var n = 8
        while (n < size) {
          n *= 2
          if (n < 0) throw new IllegalArgumentException(size.toString)
        }
        elems = new AnyRefSet[A](new Array[AnyRef](n), new Array[Byte](n), 0, 0)
      }
  
      def +=(n: A): this.type = {
        elems += n
        this
      }
  
      def clear(): Unit = elems = AnyRefSet.empty[A]
  
      def result: AnyRefSet[A] = elems
    }

  implicit def canBuildFrom[A]: CanBuildFrom[AnyRefSet[A], A, AnyRefSet[A]] =
    new CanBuildFrom[AnyRefSet[A], A, AnyRefSet[A]] {
      def apply(from: AnyRefSet[A]) = newBuilder
      def apply() = newBuilder
    }
}

final class AnyRefSet[A] private[test](as: Array[AnyRef], bs: Array[Byte], n: Int, u: Int)
  extends Function1[A, Boolean] with IterableLike[A, AnyRefSet[A]] { self =>

  private var items: Array[AnyRef] = as
  private var buckets: Array[Byte] = bs
  private var len: Int = n
  private var used: Int = u

  // hashing internals
  private var mask = items.length - 1 // size-1, used for hashing
  private var limit = (items.length * 0.65).toInt // when we should resize

  override def size: Int = len

  def +=(item: A): Boolean = {
    var i = item.## & 0x7fffffff
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
        items(j) = item.asInstanceOf[AnyRef]
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

  def -=(item: A): Boolean = {
    var i = item.## & 0x7fffffff
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

  def copy: AnyRefSet[A] = new AnyRefSet[A](items.clone, buckets.clone, len, used)

  def apply(item: A): Boolean = {
    var i = item.## & 0x7fffffff
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

  private def hash(item: A, mask0: Int, items0: Array[AnyRef], buckets0: Array[Byte]): Int = {
    var i = item.## & 0x7fffffff
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
    val nextitems = new Array[AnyRef](nextsize)
    val nextbs = new Array[Byte](nextsize)

    var i = 0
    while (i < buckets.length) {
      if (buckets(i) == 3) {
        val item = items(i)
        val j = hash(item.asInstanceOf[A], nextmask, nextitems, nextbs)
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

  override def foreach[U](f: A => U) {
    var i = 0
    while (i < buckets.length) {
      if (buckets(i) == 3) f(items(i).asInstanceOf[A])
      i += 1
    }
  }

  def iterator: Iterator[A] = new Iterator[A] {
    var epos = 0
    val elen = self.len

    var xpos = 0
    val xitems = items.clone
    val xbuckets = buckets.clone
  
    def hasNext: Boolean = epos < elen
  
    def next: A = {
      var i = xpos
      while (epos < elen) {
        val status = xbuckets(i)
        if (status == 3) {
          xpos = i + 1
          epos += 1
          return xitems(i).asInstanceOf[A]
        }
        i += 1
      }
      throw new NoSuchElementException
    }
  }

  def seq = iterator

  def newBuilder = AnyRefSet.newBuilder[A]
}
