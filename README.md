### Description

These are small, fast data structures for scalac.

`PositiveIntSet` is a fast set for storing positive `Int` values. It uses an
`Array[Int]` for its element buckets, and two `Int` fields. It uses two
sentinal values: 0 (for empty buckets) and -1 (for buckets which were
deleted).

`IntSet` is similar, but can store any `Int` value. It uses an `Array[Int]` as
well as an `Array[Byte]` to track its buckets (instead of sentinal values),
and two `Int` fields also.

`AnyRefSet` is a generic and unspecialized set that uses the same strategy as
`PositiveIntSet`, but uses null and a custom reference value as its sentinals.
It is the slowest implementation but is also the closest to the existing
`mutable.Set` implementations. Like its Scala counterpart, it cannot store null
values.

Finally, `SpecializedSet` is a generic, specialized set that uses the same
strategy as `IntSet` (a separate bucket array). Unlike `AnyRefSet` it can
store null or any other value.

The sets will grow aggressively when small: on average the buckets will be
around 42% full. `IntSet` and `SpecializedSet` will use 5/4ths of the space
that `PositiveIntSet` and `AnyRefSet` use. They are all mutable, and support
the `Iterable[A]` and `A => Boolean` interfaces.

### Testing

From within SBT, you can use the `test` command to run the ScalaCheck tests.
You can also use the `run` command to run additional randomized tests using
the `test.RandTest` target.

### Benchmarking

From within SBT, you can use the `run` command to run the Caliper benchmarks
via the `test.IntSetBenchmarks` target. The output can be processed via
`chart.sh` into a text-based table.

### Todo

1. Currently `.map` returns an `Iterable[Int]` unless the caller uses
something like `(collection.breakOut)`. It would be good to get this
returning `PositiveIntSet` or `IntSet` depending on what is appropriate.

2. It might be nice to add a faster bitset implementation as well.

3. If the numbers being used are usually going to be small and space is
important, an implementation that tries to use `Array[Byte]` or `Array[Short]`
when possible might result in substantial space savings.
