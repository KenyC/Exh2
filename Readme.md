Exh2
===========================================

Rewrite of [Exh](https://github.com/KenyC/Exh/) in Haskell.


## Goals

  - *Faster than the original.* By failure of imagination, I haven't come up with a faster algorithm for exhaustification. I'm hoping that by employing a "faster" language than Python, and some micro-optimizations here and there, I hope it will naturally come out faster.
  - *Simple design.* The original library had a number of idiosyncratic concepts (universe, predicate indices, etc). I hope this version can be used right out of the box, without as little tutorial reading as possible.
  - *Extensible.* Retain the possibility to add new constructs to  formulas and alternative ways of exhaustification. For instance, `Formula` is a wrapper for any type that implements the typeclass `IsFormula`. Users can provide their own instances of that class to create new kinds of formulas.
  - *Reliable.* Exploiting strong typing, the resulting library will not throw illegible exceptions in unexpected edge cases (unlike the original library). That and thorough unit tasty-ing.
  - *Elegant.* Just like the original library, I want formulas to be intuitive to read and write ; I'm ready to abuse the syntax for that purpose. Something like ``_A \z -> p [z] .| q [z]`` to stand for "∀z, p(z) ∨ q(z)" would be great.

## Demo

Here's how it looks. You can run this with `ghci` or `cabal repl` or `IHaskell`. Set the following extensions:

```ghci
:set -XOverloadedStrings
:set -XTypeApplications
```

Here's a simple piece of code

```haskell
p = prop "p"
q = prop "q"
r = prop "r"

-- we expect an "exactly 1" reading
f = exh $ p |. q |. r
```


All formulas are type `Formula`. They can by conjoined using `&.`, disjoined using `|.`, negated with `neg`, exhaustified with `exh`. Atomic propositions are created using `prop` ; by rule, two atoms that share the same name always evaluate to the same value. `Formula`s have `Show` instances:

 ```haskell
f
-- > Exh(p ∨ q ∨ r)
```

How do we check the result? We can draw a truth table:

```haskell
mkTruthTable [f]

{-
>
| p | q | r |Exh(p ∨ q ∨ r)|
----------------------------
| 0 | 0 | 0 |      0       |
| 0 | 0 | 1 |      1       |
| 0 | 1 | 0 |      1       |
| 0 | 1 | 1 |      0       |
| 1 | 0 | 0 |      1       |
| 1 | 0 | 1 |      0       |
| 1 | 1 | 0 |      0       |
| 1 | 1 | 1 |      0       |
-}
```

That looks right. But we can really make sure by checking for equivalence with the desired formula:

```haskell
f <=> (p &. neg q &. neg r) |. (neg p &. neg q &. r) |. (neg p &. q &. neg r)
-- > Right True
-- Returns 'Left _' if evaluation errors occurred (e.g. unvalued free variables)
```

It's superfluous at this point but we can also check that the result is incompatible with the conjunction:

```haskell
-- such a reading would be incompatible with 2 disjuncts being true
compatible f (p &. q)
-- > Right False 
```

We can delve into the details of the calculation of Exh. What are all the alternatives ? Which are innocent excludable? 

First, we pull out the internal data from the formula (of type `Exh`). `Exh` is a record containing `allAlts` and `ieAlts` as a field.


```haskell
Just exhData = getData @Exh f
-- Returns 'Nothing' if the formula isn't of the form 'Exh(...)'

-- what were the alternatives
allAlts exhData
-- > [p ∨ q ∨ r,p ∨ q ∧ r,p ∨ q,p ∨ r,p ∧ (q ∨ r),p ∧ q ∧ r,p ∧ q,p ∧ r,p,q ∨ r,q ∧ r,q,r]


-- what alternatives were innocently excludable?
ieAlts exhData
-- > [p ∧ (q ∨ r),p ∧ q ∧ r,p ∧ q,p ∧ r,q ∧ r]
```

For a better overview of the library, you can check out the documentation ; build it with `cabal haddock`.

<!-- For more, head to [demo/](https://github.com/KenyC/Exh2/tree/main/demo) -->


## Alternatives

For another Haskell implementation of IE exhaustification, also check out [exhMonad](https://github.com/patrl/exhMonad) by P. Elliott.