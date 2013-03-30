### Description

These are small, fast data structures for scalac.

`PositiveIntSet` is a fast set for storing positive `Int` values. It uses an
`Array[Int]` for its element buckets, and two `Int` fields. `IntSet` is
similar, but can store any `Int` value. It uses an `Array[Int]` as well as an
`Array[Byte]` to track its buckets, and two `Int` fields also.

The sets will grow aggressively when small: on average the buckets will be
around 42% full. `IntSet` will use 5/4ths of the space that `PositiveIntSet` does.
They are mutable, and support the `Iterable[Int]` and `Int => Boolean`
interfaces.

### Todo

1. Currently `.map` returns an `Iterable[Int]` unless the caller uses
something like `(collection.breakOut)`. It would be good to get this
returning `PositiveIntSet` or `IntSet` depending on what is appropriate.

2. It might be nice to add a faster bitset implementation as well.

3. If the numbers being used are usually going to be small and space is
important, an implementation that tries to use `Array[Byte]` or `Array[Short]`
when possible might result in substantial space savings.

4. The `IntSet` implementation could be adapted to work with any `AnyVal`.
