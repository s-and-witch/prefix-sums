## Task
Function should take a list and return another list where fist element is a sum of all elements, second should be sum of all elements withour first one and so on. Example:
```
ghci> f [1 .. 10]
[55,54,52,49,45,40,34,27,19,10]
ghci> f []
[]
ghci> f [1]
[1]
ghci> f [10, 9 .. 1]
[55,45,36,28,21,15,10,6,3,1]
```
It's not required, but good to have if function can work with infinite lists
```
ghci> length . take 20 . f [1 ...]
20
```

## Warning
Benchmark results highly depend on rts options, state of nursery and size of test data. For example if we reduce nursery size (using -A4M) and increase test data all implementations based on left scan (scanlPrelude, scanl'Prelude, scanl'Manual) becames faster then all other.

## How to run
There is environment variable `TEST_DATA_SIZE` that is used just to make dynamic data dependency to inspect how GHC works in such case. You can set it up or just ignore.

The canonical way build and run app is cabal:

```
$ TEST_DATA_SIZE=10000 cabal run prefix-sums -- --stdev 2
```
You may have problem with asm sources. In such case you can compile using ghc directly
```
$ cabal exec ghc -- -o bench -fforce-recomp -Wall -O2 -rtsopts -with-rtsopts="-A64M -AL256M -I0 -T" -fproc-alignment=64 -fllvm app/Main.hs src/Implementations.hs asm/PostfixSums.s
$ TEST_DATA_SIZE=10000 ./bench --stdev 2
```

## Benchmark results
![Results svg](./results.svg)
<img src="./results.svg">
