---
layout: post
title:  "When Howard Met Curry"
date:   2021-07-28 20:31:49
categories: logic, plt
---

The Curry-Howard correspondence is a map for moving between logic and type theory, relating propositions with types and proofs with programs. It describes a two-way street, and we can freely move between the two worlds, or perhaps merely two _perspectives_, so long as we follow the map.

Sometimes the road takes us to unexpected places. Here’s a trip I’ve been on recently.

<!--more-->

<link rel="stylesheet" type="text/css" href="https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css">

<script type="module" src="../../js/sequent.js"></script>
<script type="module" src="../../js/polarized.js"></script>

<style type="text/css">
div.connective {
  margin-bottom: 1em;
}
var[neg]::after,
var[pos]::after {
  display: inline;
}
</style>

## Double negation

I’ve been working on a language named [`sequoia`][], which embeds polarized classical logic in Haskell. Of course, Haskell corresponds to an intuitionistic logic, so we can’t just bring arbitrary classical proofs over directly. We have to use a double-negation translation, importing classical propositions <var>A</var> as intuitionistic propositions <seq-not><seq-not><var>A</var></seq-not></seq-not>. There are several such translations named (and infinitely more possible), differing in how many negations are placed where, but they all get the job done.

Curry-Howard tells us a translation for negations, but we can work this one out ourselves with a little logical knowledge: a negation <seq-not><var>A</var></seq-not> can also be encoded as the implication <seq-impl><var>A</var><seq-bottom></seq-bottom></seq-impl>. It’s straightforward enough: “<var>A</var> implies falsehood” means the same thing as “not <var>A</var>.”

Implications translate to functions, but what about <seq-bottom></seq-bottom>? That simplest, yet perhaps initially baffling, of algebraic datatypes, the empty type. We can define these for ourselves, but there’s a standard definition in the `Data.Void` module:

```haskell
data Void
```

Having no constructors, `Void` also has no inhabitants—no proof terms—just like <seq-bottom></seq-bottom>. So `Void` indeed corresponds to <seq-bottom></seq-bottom>. So what kind of thing is `a -> Void`? A function returning `Void` is a function that _cannot_ return; it can only pass control along to _another_ function returning `Void`. In other words, `a -> Void` is just what Curry-Howard tells us: a continuation.

Thus, the double negation <seq-not><seq-not><var>A</var></seq-not></seq-not> becomes a continuation from a continuation:

```haskell
type DoubleNegation a = (a -> Void) -> Void
```

which is a shape also known as continuation-passing style. Classical _languages_ embed into intuitionistic ones via CPS.

As discussed, modelling <seq-bottom></seq-bottom> with `Void` extends to modelling negations <seq-not><var>A</var></seq-not> (encoded <seq-impl><var>A</var><seq-bottom></seq-bottom></seq-impl>) with continuations. Further opens the door to using logical reasoning principles relating to <seq-bottom></seq-bottom>. For example, we can use the function `absurd`, defined as:

```haskell
absurd :: Void -> a
absurd v = case v of {}
```

to supply proofs using the principle of explosion, or ex falso quodlibet. And `Void` is an appropriately abortive substitute for <seq-bottom></seq-bottom>, since there’s no way for control to pass through a type with no inhabitants.

However good a fit `Void` might be initially, it’s quite inconvenient when embedding a language within another, whether by encoding or interpretation. You typically _want_ control to return, and to run code afterwards, and to collect results, to continue with the next iteration of a REPL, or to print, save, or transmit computed values, clean up acquired resources, and so on. Absent tricks like throwing values up the stack with exception handlers, you simply can’t: nothing returns from the `Void`.

We don’t necessarily have to use `Void` itself, however, but merely something which gives us the same proprties w.r.t. control. In particular, we can substitute `Void` out for another type, often a parameter, in our definition of `DoubleNegation`. I mentioned earlier that `DoubleNegation` is continuation-passing style, so I’m going to go ahead and spoil the punchline by renaming the type to `Cont` at the same time:

```haskell
newtype Cont r a = Cont { runCont :: (a -> r) -> r }
```

Control-wise, the main feature of `Void` is abortive control—it can never return, so it can only pass control to something else with the same property. `Cont` offers us the same behaviour, in that normal control flow is represented as applications of the continuation, passing control along to the next computation, while abortive control is possible by returning `r` directly.

By replacing `Void` with a type parameter `r` (the traditional name, perhaps for “result,”, or “return”), particularly one visible at the type level (vs. universally quantified over as with `Codensity`), we’ve also opened the door to more interesting control tricks like multi-prompt delimited control, and therefore to arbitrary effects. Handy tricks to have up your sleeve when implementing a language, to say the least.


## Beginnings and endings

I recently wrote a post about what I termed [environment-passing style][]. A series of observations arrives at (in some sense) dual translations of `a -> b` as `(b -> r) -> (a -> r)` and `(e -> a) -> (e -> b)` (shapes that you start seeing everywhere once you learn to think of them this way, e.g. in folds and unfolds, in Mendler-style algebras and coalgebras, in lambda encodings, etc.). Here, just as above, `r` substitutes for—indeed, it _abstracts_—<seq-bottom></seq-bottom>. What about `e`?

The common theme running throughout the sequent calculus is duality. `r` abstracts `Void`, `Void` corresponds to <seq-bottom></seq-bottom>, <seq-bottom></seq-bottom> (negative falsity) dualizes to <seq-one></seq-one> (positive truth), <seq-one></seq-one> corresponds to `()`, `()` is abstracted to `e`. (Back and forth ’cross the Curry-Howard bridge!)

Earlier, we judged `r` a fitting substitute for `Void` because it behaved compatibly with respect to control. In contrast, `()` doesn’t _behave_ in any way at all.

`r`’s abstraction of `Void` is determined entirely by Curry-Howard: `r` abstracts `Void` insofar as `Void` corresponds to <seq-bottom></seq-bottom> and its logical rules. The left rule has this sequent as an axiom:

<div class="connective">
  <seq-inference name="⊥" neg left>
    <seq-sequent slot="conclusion">
      <seq-gamma><seq-bottom></seq-bottom>, </seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
  </seq-inference>
</div>

This gives us ex falso quodlibet: <seq-bottom></seq-bottom> on the left suffices to prove _any_ sequent. As we saw, `Void` provides this via `absurd`. For `r`, imagine a type representing sequents implemented using `Cont`. If your arguments contain an `r`, there’s no need to consider any other arguments or what values you could compute and return; in fact, you don’t need the continuation at all. Just return the argument of type `r` and you’re done.

The right rule is instead:

<div class="connective">
  <seq-inference name="⊥" neg right>
    <seq-sequent slot="premise">
      <seq-gamma></seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma></seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta>, <seq-bottom></seq-bottom></seq-delta>
    </seq-sequent>
  </seq-inference>
</div>

This instead says that you can construct it along any sequent which was provable anyway; or, working bottom-up, <seq-bottom></seq-bottom> adds no information to a proof, and so can be discarded at any time. This one is a little stranger; we can’t construct a `Void`, period; but if we could, it certainly wouldn’t add any information. On the other hand, `r` _is_ inhabited, so we can certainly follow the analogy: we can construct an `r` if we can return without it anyway; in fact, we do, by applying the return continuation.

For `e` we instead need to trace its relationships with <seq-one></seq-one>. <seq-one></seq-one>’s rules are dual, mirror images of the ones for <seq-bottom></seq-bottom>. Again, we start with the left rule, which corresponds closely to the right rule for <seq-bottom></seq-bottom>:

<div class="connective">
  <seq-inference name="1" pos left>
    <seq-sequent slot="premise">
      <seq-gamma></seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma><seq-one></seq-one>, </seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
  </seq-inference>
</div>

Left rules can be read by recalling that a sequent is sort of like a function: we can build a function to eliminate <seq-one></seq-one> and Γ into Δ by means of a funciton eliminating Γ into Δ. Put another way, <seq-one></seq-one> doesn’t give us any power to prove things that we didn’t have without it, and so we can always introduce it as a hypothetical. `()` works much the same way: adding an argument of type `()` won’t help you construct anything, since you could have just introduced it as a literal. `e` therefore must work the same way, but we’re going to weaken our informal restatement of this down to: you are free to ignore an argument of type `e`. Of course, we aren’t in an e.g. linear setting, so we’re free to ignore _every_ argument. But even if we were in a linear setting, `e` should still be discardable.

On the right:

<div class="connective">
  <seq-inference name="1" pos right>
    <seq-sequent slot="conclusion">
      <seq-gamma></seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta>, <seq-one></seq-one></seq-delta>
    </seq-sequent>
  </seq-inference>
</div>

You can always make a <seq-one></seq-one>. Ditto `()`; it’s what makes it discardable. (<seq-one></seq-one>, a positive connective, is indeed defined by its right rule.) `e`, then, must also be freely introduced, ambiently, ubiquitously.

Considering that we started with the humble <seq-bottom></seq-bottom> and <seq-one></seq-one>, the combined consequences of these rules and equivalences for our purposes are surprisingly useful. Expressed as derived rules, on the <seq-bottom></seq-bottom> side, if we have an A, we can use it to eliminate an <seq-impl><var pos>A</var><seq-bottom><sub slot="op-decoration" class="neg">R</sub></seq-bottom></seq-impl>—a continuation—at any time; likwise, we can introduce a continuation from A to satisfy a demand for a A:

<div class="connective">
  <seq-inference left>
    <seq-sequent slot="premise">
      <seq-gamma></seq-gamma>
      <seq-turnstile><sub slot="decoration" class="neg">R</sub></seq-turnstile>
      <seq-delta>, <var pos>A</var></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma><seq-impl><var pos>A</var><seq-bottom><sub slot="op-decoration" class="neg">R</sub></seq-bottom></seq-impl>, </seq-gamma>
      <seq-turnstile><sub slot="decoration" class="neg">R</sub></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
  </seq-inference>
  <seq-inference right>
    <seq-sequent slot="premise">
      <seq-gamma><var pos>A</var>, </seq-gamma>
      <seq-turnstile><sub slot="decoration" class="neg">R</sub></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma></seq-gamma>
      <seq-turnstile><sub slot="decoration" class="neg">R</sub></seq-turnstile>
      <seq-delta>, <seq-impl><var pos>A</var><seq-bottom><sub slot="op-decoration" class="neg">R</sub></seq-bottom></seq-impl></seq-delta>
    </seq-sequent>
  </seq-inference>
</div>

Dually, demand for <seq-impl><seq-one><sub slot="op-decoration" class="pos">E</sub></seq-one><var neg>A</var></seq-impl> is satisfied by demand for <var neg>A</var>; and we can always turn <var neg>A</var> into <seq-impl><seq-one><sub slot="op-decoration" class="pos">E</sub></seq-one><var neg>A</var></seq-impl>:

<div class="connective">
  <seq-inference left>
    <seq-sequent slot="premise">
      <seq-gamma></seq-gamma>
      <seq-turnstile><sub slot="decoration" class="pos">E</sub></seq-turnstile>
      <seq-delta>, <var pos>A</var></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma><seq-impl><seq-one><sub slot="op-decoration" class="pos">E</sub></seq-one><var neg>A</var></seq-impl>, </seq-gamma>
      <seq-turnstile><sub slot="decoration" class="pos">E</sub></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
  </seq-inference>
  <seq-inference right>
    <seq-sequent slot="premise">
      <seq-gamma><var pos>A</var>, </seq-gamma>
      <seq-turnstile><sub slot="decoration" class="pos">E</sub></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma></seq-gamma>
      <seq-turnstile><sub slot="decoration" class="pos">E</sub></seq-turnstile>
      <seq-delta>, <seq-impl><seq-one><sub slot="op-decoration" class="pos">E</sub></seq-one><var neg>A</var></seq-impl></seq-delta>
    </seq-sequent>
  </seq-inference>
</div>


## Assertive negativity

R represents the ubiquitously available ability to jump to the end of the program and abort. E, on the other hand, represents the ubiquitously available ability to summon (already globally-available) information out of thin air: the environment (hence the name E). Neither of these give us anything _really_ new—after all, we could always pass information inwards, threading it through all the intervening calls, or abort and return outwards by means of `Maybe` or the like. But doing either in a single step _without_ changing the rest of the code base is pretty handy.

Further, `Cont r a` gives us some tools that <seq-not><seq-not><var>A</var></seq-not></seq-not> alone does not, including delimited continuations. Delimited continuations allow us to jump not only to the end of the program, but to some designated intermediate position(s)—often called prompts—introduced by `reset`, and even to resume control at the point at which we jumped afterwards. This in turn allows us to encode arbitrary effects and handlers.

In much the same way, the dual structure—probably a comonad—gives us local environments, sandboxing, and coeffects.

If `Cont r a` is ¬¬A, then what is this dual structure? Following the thread backwards, `Cont r a` is <seq-not><seq-not><var>A</var></seq-not></seq-not> because `Cont r a` is <seq-impl><span>(<seq-impl><var>A</var><seq-bottom><sub slot="op-decoration" class="neg">R</sub></seq-bottom></seq-impl>)</span><seq-bottom><sub slot="op-decoration" class="neg">R</sub></seq-bottom></seq-impl>, which is an encoding of <seq-not><seq-not><var>A</var></seq-not></seq-not>. Our encoding of <seq-impl><seq-one><sub slot="op-decoration" class="pos">E</sub></seq-one><var neg>A</var></seq-impl>, on the other hand, doesn’t correspond to any connective—yet. So let’s introduce one: <seq-not-untrue></seq-not-untrue>, pronounced “not untrue,” is an _assertion_ (some relation to the logical notion, no relation to the computational one), dual to a negation, and works just like our encoding above:

<div class="connective">
  <seq-inference name='¬̷' neg left>
    <seq-sequent slot="premise">
      <seq-gamma><seq-focus><var neg>A</var></seq-focus> </seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma><seq-focus><seq-not-untrue><var neg>A</var></seq-not-untrue></seq-focus> </seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
  </seq-inference>
  <seq-inference name='¬̷' neg right>
    <seq-sequent slot="premise">
      <seq-gamma></seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta>, <var neg>A</var></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma></seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta>, <seq-not-untrue><var neg>A</var></seq-not-untrue></seq-delta>
    </seq-sequent>
  </seq-inference>
</div>

Unlike <seq-not></seq-not>, the composition of <seq-not-untrue></seq-not-untrue> on itself is surprisingly boring. If <seq-not-untrue></seq-not-untrue> encodes as <seq-one></seq-one> → A, then all we’ve got is <seq-one></seq-one> → <seq-one></seq-one> → A, which gives us nothing the single instance didn’t. I thought these things were supposed to be dual; what gives?


## Polarization

I mentioned before that `sequoia` embeds _polarized_ classical logic. Thus, the above tells only half the story, because we have two different negations: the negative negation ¬ (“not”), and the positive negation ~ (“negate”). They further accept propositions of the opposite polarity, i.e. ¬ takes a positive proposition and ~ takes a negative one, and are involutive, cancelling each other out. ¬~A<sup>-</sup> ≈ A<sup>-</sup>, and ~¬A<sup>+</sup> ≈ A<sup>+</sup>.

Likewise, there are actually two different assertions. <seq-not-untrue></seq-not-untrue>, which we saw above, is the negative one, while the positive one is stranger still. We arrived at the negative assertion by considering the negative negation, maybe we can find the positive one by a similar route.

The encoding of ¬A as A → <seq-bottom></seq-bottom> which we saw earlier wouldn’t be well-polarized for the positive negation ~A. Instead, ~A is encoded as <seq-one></seq-one> − A, where A − B (“A without B”) is (categorically) a coexponential, (logically) a coimplicaiton or subtraction, and (computationally) a calling context. (Downen has called it a call stack, but I dislike that name, as it’s more like a single frame than an entire stack.)

While the logical rules for its introduction and elimination offer some insight into what its representation must hold, it’s perhaps clearest under a different set of encodings: this time, encoding → and − in terms of disjunction/conjunction and negations. Classically, A → B can be encoded as ¬A ∨ B, while A − B can be encoded as A ∧ ¬B (i.e. “A and not B,” hence the pronunciation of − as “without”). If A − B could be encoded as a conjunction of A and the negation of B, then what does Curry-Howard have to say about that? Conjunctions are product types; negations are still continuations; A − B is isomorphic to a pair of an A and a continuation from B.

We can see now that A → B and A - B are dual: A − B holds both the argument to and continuation from A → B. I sometimes imagine A − B as a pipeline, with a section missing; A → B is precisely the missing section that fits and completes it.

Thus far our encodings of the two negations and our single assertion are:

- <seq-not><var pos>A</var></seq-not> ≈ <seq-impl><var pos>A</var><seq-bottom></seq-bottom></seq-impl>
- <seq-negate><var neg>A</var></seq-negate> ≈ <seq-coimpl><seq-one></seq-one><var neg>A</var></seq-coimpl>
- <seq-not-untrue><var neg>A</var></seq-not-untrue> = <seq-impl><seq-one></seq-one><var neg>A</var></seq-impl>

We can further organize these by polarity and purpose:

<table>
  <thead>
    <tr>
      <th>
      </th>
      <th>
        negative
      </th>
      <th>
        positive
      </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>
        negation
      </th>
      <td>
        <seq-not><var pos>A</var></seq-not> ≈ <seq-impl><var pos>A</var><seq-bottom></seq-bottom></seq-impl>
      </td>
      <td>
        <seq-negate><var neg>A</var></seq-negate> ≈ <seq-coimpl><seq-one></seq-one><var neg>A</var></seq-coimpl>
      </td>
    </tr>
    <tr>
      <th>
        assertion
      </th>
      <td>
        <seq-not-untrue><var neg>A</var></seq-not-untrue> = <seq-impl><seq-one></seq-one><var neg>A</var></seq-impl>
      </td>
      <td>
        …?
      </td>
    </tr>
  </tbody>
</table>

The negations both invert polarity, whereas ¬̷ maintains it. Further, the negative connectives both employ →, whereas ~ uses −. Putting it together, we can expect the positive assertion to encode as −, and to maintain polarity, and that gives us:

<table>
  <thead>
    <tr>
      <th>
      </th>
      <th>
        negative
      </th>
      <th>
        positive
      </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>
        negation
      </th>
      <td>
        <seq-not><var pos>A</var></seq-not> ≈ <seq-impl><var pos>A</var><seq-bottom></seq-bottom></seq-impl>
      </td>
      <td>
        <seq-negate><var neg>A</var></seq-negate> ≈ <seq-coimpl><seq-one></seq-one><var neg>A</var></seq-coimpl>
      </td>
    </tr>
    <tr>
      <th>
        assertion
      </th>
      <td>
        <seq-not-untrue><var neg>A</var></seq-not-untrue> = <seq-impl><seq-one></seq-one><var neg>A</var></seq-impl>
      </td>
      <td>
        <seq-true><var pos>A</var></seq-true> ≈ <seq-coimpl><var pos>A</var><seq-bottom></seq-bottom></seq-coimpl>
      </td>
    </tr>
  </tbody>
</table>

✓, pronounced “true,” is truly dual to ¬, and ¬̷ is dual to ~. ✓’s encoding gives us the following rules:

<div class="connective">
  <seq-inference name="✓" pos left>
    <seq-sequent slot="premise">
      <seq-gamma><var pos>A</var>, </seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma><seq-true><var pos>A</var></seq-true>, </seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta></seq-delta>
    </seq-sequent>
  </seq-inference>
  <seq-inference name="✓" pos right>
    <seq-sequent slot="premise">
      <seq-gamma></seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta> <seq-focus><var pos>A</var></seq-focus></seq-delta>
    </seq-sequent>
    <seq-sequent slot="conclusion">
      <seq-gamma></seq-gamma>
      <seq-turnstile></seq-turnstile>
      <seq-delta> <seq-focus><seq-true><var pos>A</var></seq-true></seq-focus></seq-delta>
    </seq-sequent>
  </seq-inference>
</div>

So far, so… disappointing. These are precisely the same rules as we found for ¬̷; only the symbols and the polarities have been swapped. And what’s worse, the same was already true of the rules for the negations.

Speaking of which, the encoding for ~ seems circular: the positive continuation ~A can be encoded as <seq-one></seq-one> − A, itself represented as a pair of a unit value and … a continuation from A? But no, it’s the positive _negation_ ~A that can be encoded thus. Just the same, that distinction alone isn’t satisfying: one wonders what the point of ~ is, polarity aside. We’ve already got a continuation connective in ¬; what do we need another one for?

It was in precisely such a mood that I happened to open Paul Downen & Zena Ariola’s recent paper _[Compiling with Classical Connectives][]_ to where I’d last left off, on a page starting with this paragraph:

<figure>
  <blockquote cite="https://lmcs.episciences.org/6740"><p>The two negation types can be thought of as two dual way for representing first-class continuations in a programming language. One way to formulate a continuation is by capturing the context as a first class value. This corresponds to the data type ⊖A which packages up a covalue F as the value ⊖F, which can be later unpacked by pattern-matching in λ⊖α.c. Another way to formulate continuations is through functions that never return. This corresponds to the codata type ¬A which has values of the form λ¬x.c, which is analogous to a function abstraction that does not bind a return pointer, and covalues of the form ¬W, which is analogous to throwing a value to a continuation without waiting for a returned result.</p></blockquote>
  <figcaption><cite><a href="https://lmcs.episciences.org/6740">Compiling with Classical Connectives</a></cite>, Paul Downen, Zena M. Ariola</figcaption>
</figure>

In short, you don’t use the same representation of continuations under two different names; you use two different representations, each with their own strengths. I was delighted to read this, because it reflects something about ~ that I’d only just noticed a day or two prior: the reason representing ¬A with continuations `a -> r` _works_ is that we’re consistent about it. Shouldn’t we be consistent about our treatment of <seq-one></seq-one>, too? In which case, we should revisit our table:

<table>
  <thead>
    <tr>
      <th>
      </th>
      <th>
        negative
      </th>
      <th>
        positive
      </th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>
        negation
      </th>
      <td>
        ¬A ≈ A → <seq-bottom></seq-bottom><sub>R</sub>
      </td>
      <td>
        ~A ≈ <seq-one></seq-one><sub>E</sub> - A
      </td>
    </tr>
    <tr>
      <th>
        assertion
      </th>
      <td>
        ¬̷A = <seq-one></seq-one><sub>E</sub> → A
      </td>
      <td>
        ✓A ≈ A - <seq-bottom></seq-bottom><sub>R</sub>
      </td>
    </tr>
  </tbody>
</table>

In other words, ~A is defined to hold the environment in which it was constructed. It’s a lot like a closure, and precisely the kind of thing Downen & Ariola describe.

This also sheds some light on the contrast between the two assertions. ✓A holds an A, along with a continuation from R. (I’m not exactly certain what that means, just yet.) ¬̷A, on the other hand, may close over an A, or it may _construct_ one from E. In other words, ¬̷A models dynamically-scoped variables, and ✓A models lexically-scoped ones. And while ¬̷A may look partial at first blush, it‘s important to remember that while the logical rules don’t (currently) encode this fact, E needn’t be the same for every sequent any more than R need be. (`reset`, for example, allows the conclusion’s R to vary from the premise’s.)


## Endings and beginnings

I set out to understand continuation-passing style better, and whatever its opposite might be, and ended up coming up with two new connectives modelling lexical & dynamic scoping, a way to integrate coeffects with the same deep level of access as effects and CPS already enjoy, and a deeper appreciation the positive negation ~ and its role in contrast to ¬. Given all that, it’s hard to be too sad that I still don’t know much about what I was calling [environment-passing style][]. I intend to carry the even-handed treatment of ~ further still, and try to understand the encodings of → and −. I also intend to write about the modular, composable representation of n-input, n-output computations I’ve come up with based on the above findings. That will have to wait.

For now, I mostly just wanted to share how wonderful it is what sort of things we can discover as we follow the road back and forth across the Curry-Howard correspondence.

<aside>
  <p>Thanks to Peter Murphy, <a href="https://twitter.com/ptrfrncsmrph/status/1420749056125685762">CSS Hero</a>, for help with the CSS for the sequent calculus rules.</p>
</aside>

[`sequoia`]: https://github.com/robrix/sequoia
[Compiling with Classical Connectives]: https://lmcs.episciences.org/6740
[environment-passing style]: /posts/2021-07-08-environment-passing-style/
