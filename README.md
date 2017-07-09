Implementations of various SAT solving algorithms, mostly in Haskell. These are just to
get a feel for the problem, they are *not* intended as real world solutions!

* DPLL : straightforward implementation of DPLL as described on [Wikipedia](https://en.wikipedia.org/wiki/DPLL_algorithm).
* DPLLopt : same as above, but with some command line options and an internal timer.
* DPLL_Vector : using vectors rather than lists, avoiding list comprehensions and attoparsec as the
    parser, otherwise exactly the same implementation as those above. The speed up is quite
    remarkable.
