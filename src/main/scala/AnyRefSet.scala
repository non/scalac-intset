package test

import scala.collection.IterableLike
import scala.collection.mutable.{Builder, GrowingBuilder, MapBuilder}
import scala.collection.generic.CanBuildFrom

import scala.reflect.ClassTag

object AnyRefSet {
  def empty[A]() = new AnyRefSet[A](new Array[AnyRef](8), 0, 0)

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
        elems = new AnyRefSet[A](new Array[AnyRef](n), 0, 0)
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


final class AnyRefSet[A] private[test](as: Array[AnyRef], n: Int, u: Int)
  extends Function1[A, Boolean] with Iterable[A] with IterableLike[A, AnyRefSet[A]] { self =>

  private final val blocked = new Object

  private var items: Array[AnyRef] = as
  private var len: Int = n
  private var used: Int = u

  // hashing internals
  private var mask = items.length - 1 // size-1, used for hashing
  private var limit = (items.length * 0.65).toInt // when we should resize

  override def size: Int = len

  def +(item: A): AnyRefSet[A] = {
    val set = this.copy
    set += item
    set
  }

  def +=(item: A): Boolean = {
    if (item == null) throw new IllegalArgumentException("cannot insert null")
    var i = item.## & 0x7fffffff
    var perturbation = i
    var fresh = true
    while (true) {
      val j = i & mask
      var found = items(j)
      if (found == blocked) {
        if (apply(item)) return false
        found = null
        fresh = false
      }

      if (found != null) {
        if (found == item) {
          return false
        } else {
          i = (i << 2) + i + perturbation + 1
          perturbation = perturbation >> 5
        }
      } else {
        items(j) = item.asInstanceOf[AnyRef]
        len += 1
        if (fresh) {
          used += 1
          if (used > limit) resize()
        }
        return true
      }
    }
    false // impossible
  }

  def ++=(rhs: Traversable[A]): Unit = rhs.foreach(this += _)

  def -(item: A): AnyRefSet[A] = {
    val set = this.copy
    set -= item
    set
  }

  def -=(item: A): Boolean = {
    var i = item.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val found = items(j)
      if (found == null) {
        return false
      } else if (found == item) {
        items(j) = blocked
        len -= 1
        return true
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    false // impossible
  }

  def --(rhs: Traversable[A]): AnyRefSet[A] = {
    val set = this.copy
    rhs.foreach(set -= _)
    set
  }

  def --=(rhs: Traversable[A]): Unit = rhs.foreach(this -= _)

  def |(rhs: AnyRefSet[A]): AnyRefSet[A] =
    if (size >= rhs.size) {
      val set = this.copy
      rhs.foreach(set += _)
      set
    } else {
      val set = rhs.copy
      this.foreach(set += _)
      set
    }

  def |=(rhs: AnyRefSet[A]): Unit = rhs.foreach(this += _)

  def &(rhs: AnyRefSet[A]): AnyRefSet[A] = {
    val set = AnyRefSet.empty[A]
    if (size <= rhs.size)
      this.foreach(n => if (rhs(n)) set += n)
    else
      rhs.foreach(n => if (this(n)) set += n)
    set
  }

  def &=(rhs: AnyRefSet[A]): Unit = {
    val set = this & rhs
    items = set.items
    len = set.len
    used = set.used
    mask = set.mask
    limit = set.limit
  }

  def copy: AnyRefSet[A] = new AnyRefSet[A](items.clone, len, used)

  private[test] def unsafeApply(item: AnyRef): Boolean = {
    var i = item.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask
      val found = items(j)
      if (found == null) {
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

  def apply(item: A): Boolean = unsafeApply(item.asInstanceOf[AnyRef])

  private def hash(item: A, mask0: Int, items0: Array[AnyRef]): Int = {
    var i = item.## & 0x7fffffff
    var perturbation = i
    while (true) {
      val j = i & mask0
      val found = items0(j)
      if (found == null || found == item) {
        return j
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
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

    val b = blocked
    var i = 0
    while (i < items.length) {
      val found = items(i)
      if (found != null && found != b) {
        val j = hash(found.asInstanceOf[A], nextmask, nextitems)
        nextitems(j) = found
      }
      i += 1
    }

    items = nextitems
    mask = nextmask
    limit *= factor
  }

  override def foreach[U](f: A => U) {
    var i = 0
    val b = blocked
    while (i < items.length) {
      val found = items(i)
      if (found != null && found != b) f(found.asInstanceOf[A])
      i += 1
    }
  }

  def iterator: Iterator[A] = new Iterator[A] {
    var epos = 0
    val elen = self.len

    var xpos = 0
    val xitems = items.clone
    val xb = blocked
  
    def hasNext: Boolean = epos < elen
  
    def next: A = {
      var i = xpos
      while (epos < elen) {
        val found = xitems(i)
        if (found != null && found != xb) {
          xpos = i + 1
          epos += 1
          return found.asInstanceOf[A]
        }
        i += 1
      }
      throw new NoSuchElementException
    }
  }

  override def newBuilder = AnyRefSet.newBuilder[A]

  override def equals(rhs: Any) = rhs match {
    case set: AnyRefSet[_] =>
      size == set.size && forall(a => set.unsafeApply(a.asInstanceOf[AnyRef]))
    case _ =>
      false
  }
}
