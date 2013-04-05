package test

import scala.collection.mutable.{Builder, GrowingBuilder, MapBuilder}
import scala.collection.generic.CanBuildFrom

// stand-in for the real symbol object
case class Symbol(id: Int, name: String)

object Symbol {
  private var currId: Int = 0
  def apply(name: String) = {
    currId += 1
    new Symbol(currId, name)
  }
}


object SymbolSet {
  def empty() = new SymbolSet(new Array[Int](8), new Array[Symbol](8), 0, 0, 0)

  def apply(syms: Symbol*) = {
    val set = empty
    syms.foreach(set += _)
    set
  }
  
  def newBuilder: Builder[Symbol, SymbolSet] =
    new Builder[Symbol, SymbolSet] {
      private var elems: SymbolSet = SymbolSet.empty

      override def sizeHint(size: Int) {
        var n = 8
        while (n < size) {
          n *= 2
          if (n < 0) throw new IllegalArgumentException(size.toString)
        }
        elems = new SymbolSet(new Array[Int](n), new Array[Symbol](n), 0, 0, 0)
      }
  
      def +=(symbol: Symbol): this.type = {
        elems += symbol
        this
      }
  
      def clear(): Unit = elems = SymbolSet.empty
  
      def result: SymbolSet = elems
    }
  
  implicit def canBuildFrom: CanBuildFrom[SymbolSet, Symbol, SymbolSet] =
    new CanBuildFrom[SymbolSet, Symbol, SymbolSet] {
      def apply(from: SymbolSet) = newBuilder
      def apply() = newBuilder
    }
}

final class SymbolSet private[test] (as: Array[Int], syms: Array[Symbol], n: Int, u: Int, si: Int)
  extends Function1[Symbol, Boolean] with Iterable[Symbol] { self =>

  private[test] var symi: Int = si
  private[test] var symbols: Array[Symbol] = syms

  private[test] var items: Array[Int] = as
  private[test] var len: Int = n // how many elements are in the set
  private[test] var used: Int = u // how many slots are currently in use

  // hashing internals
  private[test] var mask = items.length - 1 // size-1, used for hashing
  private[test] var limit = (items.length * 0.65).toInt // resize at this point

  final override def size: Int = len

  final def +(symbol: Symbol): SymbolSet = {
    val set = this.copy
    set += symbol
    set
  }

  final def +=(symbol: Symbol): Boolean = {
    val item = symbol.id
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
        if (found < 0 && apply(symbol)) return false
        items(j) = item
        len += 1
        if (found == 0) {
          used += 1
          if (used > limit) resize()
        }

        if (symi >= symbols.length)
          symbols = java.util.Arrays.copyOf(symbols, symbols.length * 2)
        symbols(symi) = symbol
        symi += 1

        return true
      }
    }
    false // impossible
  }

  def ++(rhs: Traversable[Symbol]): SymbolSet = {
    val set = this.copy
    rhs.foreach(set += _)
    set
  }

  def ++=(rhs: Traversable[Symbol]): Unit = rhs.foreach(this += _)

  final def -(symbol: Symbol): SymbolSet = {
    val set = this.copy
    set -= symbol
    set
  }

  def -=(symbol: Symbol): Boolean = {
    val item = symbol.id
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
        var k = 0
        while (k < symi && symbols(k).id != item) k += 1
        if (k < symi - 1) {
          System.arraycopy(symbols, k + 1, symbols, k, symi - k - 1)
        }
        symi -= 1
        return true
      } else {
        i = (i << 2) + i + perturbation + 1
        perturbation = perturbation >> 5
      }
    }
    false // impossible
  }

  def --(rhs: Traversable[Symbol]): SymbolSet = {
    val set = this.copy
    rhs.foreach(set -= _)
    set
  }

  def --=(rhs: Traversable[Symbol]): Unit = rhs.foreach(this -= _)

  def |(rhs: SymbolSet): SymbolSet =
    if (size >= rhs.size) {
      val set = this.copy
      rhs.foreach(set += _)
      set
    } else {
      val set = rhs.copy
      this.foreach(set += _)
      set
    }

  def |=(rhs: SymbolSet): Unit = rhs.foreach(this += _)

  def &(rhs: SymbolSet): SymbolSet = {
    val set = SymbolSet.empty
    if (size <= rhs.size)
      this.foreach(sym => if (rhs(sym)) set += sym)
    else
      rhs.foreach(sym => if (this(sym)) set += sym)
    set
  }

  def &=(rhs: SymbolSet): Unit = {
    val set = this & rhs
    items = set.items
    len = set.len
    used = set.used
    mask = set.mask
    limit = set.limit
  }

  final def copy: SymbolSet = new SymbolSet(items.clone, symbols.clone, len, used, symi)

  def apply(symbol: Symbol): Boolean = {
    val item = symbol.id
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

  override def foreach[U](f: Symbol => U): Unit = {
    var i = 0
    while (i < symi) {
      f(symbols(i))
      i += 1
    }
  }

  def iterator: Iterator[Symbol] = new Iterator[Symbol] {
    var i = 0
    val syms = java.util.Arrays.copyOf(self.symbols, self.symbols.length)
    val limit = self.symi
  
    def hasNext: Boolean = i < limit
  
    def next: Symbol = if (i < limit) {
      val sym = syms(i)
      i += 1
      sym
    } else {
      throw new NoSuchElementException
    }
  }

  override def equals(rhs: Any) = rhs match {
    case set: SymbolSet =>
      size == set.size && forall(set.apply)
    case _ =>
      false
  }
}
