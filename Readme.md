Exh2
===========================================

Rewrite of [Exh](https://github.com/KenyC/Exh/) in Haskell.

(For another Haskell implementation, also check out [exhMonad](https://github.com/patrl/exhMonad) by P. Elliott)

**Goals:**

  - *Faster than the original.* By failure of imagination, I haven't come up with a faster algorithm for exhaustification. I'm hoping that by employing a "faster" language than Python, and some micro-optimizations here and there, I hope it will naturally come out faster.
  - *Simple design.* The original library had a number of idiosyncratic concepts (universe, predicate indices, etc). I hope this version can be used right out of the box, without as little tutorial reading as possible.
  - *Extensible.* Retain the possibility to add new constructs to  formulas and alternative ways of exhaustification. For instance, `Formula` is a wrapper for any type that implements the typeclass `IsFormula`. Users can provide their own instances of that class to create new kinds of formulas.
  - *Reliable.* Exploiting strong typing, the resulting library will not throw illegible exceptions in unexpected edge cases (unlike the original library). That and thorough unit tasty-ing.
  - *Elegant.* Just like the original library, I want formulas to be intuitive to read and write ; I'm ready to abuse the syntax for that purpose. Something like ``_A \z -> p [z] .| q [z]`` to stand for "∀z, p(z) ∨ q(z)" would be great.