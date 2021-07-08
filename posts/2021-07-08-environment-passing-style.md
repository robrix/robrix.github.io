---
layout: post
title:  "Environment-Passing Style"
date:   2021-07-08 10:08:54
categories: logic, plt
---

Functions of type A → B can be translated into corresponding functions of type ¬B → ¬A in continuation-passng style (CPS), where ¬A is logically negation but computationally a continuation from A.

<!--more-->

This widens the view of functions as value transformers, taking values of type A to values of type B, to include an alternative perspective of them as continuation transformers (as noted by Andrzej Filinski in _Declarative Continuations and Categorical Duality_.)

Logic and computation are [rife with dualities][], leading one to wonder: what’s the dual of CPS?

Logically, ¬A is often regarded as equivalent to, or even synonymous with, A → ⊥. Computationally, ⊥ corresponds under Curry-Howard to the empty type (sometimes written 0; named `Void` in ghc Haskell). This is a perfectly reasonable choice when the entire program will be under this translation, but we often want only part of the program to undergo translation and thus a type more tractable than `Void` would be welcome.

Fortunately, it’s entirely acceptable to choose some _other_ type to substitute for ⊥, so lng as we’re consistent. Somewhat arbitrarily, we’ll call R the result type. Now our CPS translation yields:

(B → R) → (A → R)

As a quick aside: this representation is just a flip away from being the composition of the `ReaderT` monad transformer on `ContT`.

As Filinski notes, functions-as-continuation-transformers map continuation contravariantly from the return type to the argument type. Otherwise put, applying a function f : A -> B to a continuation k : ¬B (an operation which Filinski writes as k ↓ f) yields a continuation of type ¬A. Thinking of continuations as functions to some specially-chosen return type maks this look rather more like composition than application, but Filinski further stresses the importance of understanding continuations as their own kind of thing.

That said, it raises another question: why are continuations written with a type constructor, while values just sort of _are_? Why the asymmetry?

As noted before, ¬B → ¬A is equivalent to (B → ⊥) → (A → ⊥). One way to approach the question of duality is to dualize the logical propositions. We’re going to use the polarized logic from the previous post, but we won’t worry _too_ much about the polarities, simply acknowledging that some quantity of shifts will be necessary.


Implication dualizes to subtraction: (A → B)<sup>⊥</sup> = A - B. Note that this is dual all by itself—A and B aren’t negated. So one answer could be: the dual of ¬B → ¬A is ¬B - ¬A. This is true; the latter represents precisely the negative space (as it were) around the former. But it’s somwhat unsatisfying just the same; that’s the dual of the function type, not the dual of CPS as a mechanism.

We can also negate the argument and return types, in which case the dual of ¬B → ¬A could be taken to be either B → A (classically) or ¬¬B → ¬¬A. And since A → B is equivalent to ¬B → ¬A, that means that we can further replace ¬¬B → ¬¬A with ¬¬¬A → ¬¬¬B, and then apply triple negation elimination (both classically and intuitionistically valid) to obtain (¬B → ¬A)<sup>⊥</sup> = ¬A → ¬B. Also true; also unsatisfying. What’s missing?

One of the great benefits of programmer-accessible CPS is the availability of delimited continuations, which are a generalization of regular, no-return continuations to return to one or more chosen places in a computation (typically in an outer scope). They furthermore allow the code there to return back into the inner scope, and thus enable inner scopes to communicate with outer ones—exactly what’s needed for effect handlers. ¬A _represents_ a continuation from A, in that we interpret it as one, but if we want the dual of CPS we need to dualize continuations, too.

As one further bit of inspiration, just as ¬A, the negative negation of a positive proposition A, is equivalent to A → ⊥, ~A, the positive negation of a negative proposition A, is equivalent to 1 - A. The data of a subtraction consists of an argument on the left and a continuation from the proposition on the right—the argument to and continuation from the result of the dual function type, precisely—so using subtractions themselves would be moving the problem around, to some degree. But note that 1 and ⊥ are de Morgan duals; so clearly there’s something here.

Thus, modulo a bit of hand-waving, we arrive at:

((B → ⊥) → (A → ⊥))<sup>⊥</sup> = (1 → A) → (1 → B)

And just as we were justified in replacing ⊥ with R, we can now feel justified in replacing 1 with S, yielding:

((B → R) → (A → R))<sup>⊥</sup> = (S → A) → (S → B)

(The justification being that S abstracts 1 just as R abstracts ⊥.)

I’m describing this pattern of function as environment-passing style (tho I’m hoping someone has already described this in detail under a rather better name). I don’t have good idioms for its use, no handy APIs, nor best practices, but I am hopeful that it will prove useful in exploring the relationships between profunctor optics and CPS, and in describing a program’s context-awareness à la coeffects, just like continuations have done for effects. Programmable continuations have been marvellously handy; perhaps programmable values will be as well. And the composition(s) of the two styles is especially intruiging given that values and continuations can each wrap the other.

Finally, a vivid analogy keeps popping into my head, of a piece of twine or wire with 1 at one end and ⊥ at the other, and a program’s interpreter either pulling itself along generating values, or pushing itself along satisfying continuations. Good luck, little program.

[rife with dualities]: /posts/2021-06-07-duality/
