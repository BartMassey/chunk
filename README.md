# Data.List.chunk

This Haskell utility `chunk` function is proposed for
inclusion in `Data.List`. The `chunk` function groups a list
up into chunks of specified size.

    > chunk 3 [1..8]
    [[1,2,3],[4,5,6],[7,8]]

The performance of `chunk` is *O(n)* in the size of the
input list and *O(1)* in the chunk size, with good
constants. [Criterion](http://www.serpentine.com/criterion/)
benchmarks are included here.

The code is believed to be correct, by inspection and test.
[HUnit](http://sourceforge.net/projects/hunit/) unit tests
are included here.

By construction and according to the tests here, `chunk` is
fully lazy.

To build everything -- code, benchmark results
(`criterion.html`), unit test results (`hunit.txt`) and
[Haddock](https://www.haskell.org/haddock/) documentation
(`doc/index.html`) -- just say `make` here.

This work is licensed under the "3-clause BSD License".
Please see the file LICENSE in the source
distribution of this software for license terms.
