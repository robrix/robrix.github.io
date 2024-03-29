---
layout: post
title:  "All you need is λ, part one: booleans"
date:   2020-03-29 20:17:26
categories: lambda calculus, plt
---

Nearly a century ago, Alonzo Church invented the simple, elegant, and yet elusive lambda calculus. Along with Alan Turing, he then proved the Church-Turing thesis: that anything computable with a Turing machine can also be computed in the lambda calculus. However, nearly as soon as we had digital computers, we started inventing programming languages, and with them a vast treasure of features, beautiful and terrible, many of which seem very hard to relate to the fundamental nature of computability, let alone the lambda calculus specifically.

<!--more-->

While it’s true that anything which can be computed, period, can be computed in the lambda calculus, you might not want to: it’s austere, to say the least, and was not designed with modern sensibilities regarding readability in mind. We developed all those languages and features for a reason! Still, Church demonstrated not just that it was possible to compute anything computable with the lambda calculus, but also _how_ one might do so.

In this series, we’ll examine some ways to express common programming language features using the minimalistic tools of the lambda calculus. We begin with perhaps the most ubiquitous type: booleans.


## λ is blind

The lambda calculus’s austerity is extreme: you don’t even have booleans. All you have are:

1. Lambda abstractions;

2. Applications; and

3. Variables.

We’ll now review these in some detail; feel free to skip this section if you’re already familiar with the lambda calculus.


### Lambda abstractions

Lambda abstractions (“lambdas,” “abstractions,” and “functions” will also be used interchangeably) introduce a function of a single variable.

Abstractions are written `λ x . y`, for variable `x` and expression `y`, where `x` is now available as a bound variable in the body, and any enclosing definition of `x` is shadowed (i.e. `λ x . λ x . x` = `λ x . λ y . y` ≠ `λ x . λ y . x`). (We shall assume strictly lexical scoping for the time being.)

In Haskell, we would write `\ x -> y` instead; in JavaScript, `function (x) { return y }` or `(x) => y`.


### Applications

Applications (“function application” and “function call” will be used interchangeably) apply the result of the expression on the left to the expression on the right.

Applications are written as `x y`, for expressions x and y, and left-associated, i.e. `a b c` = `(a b) c` ≠ `a (b c)`. Function application binds tighter than lambda abstraction, i.e. `λ x . λ y . y x` = `λ x . λ y . (y x)` ≠ `λ x . (λ y . y) x`.

The syntax is the same in Haskell; in JavaScript, we would write `x(y)` or `a(b, c)`. Note however that since lambda calculus functions are all single-argument functions, a more direct (though less idiomatic) equivalent for the latter would be `a(b)(c)`.


### Variables

Variables introduced by enclosing lambdas.

Variable are written as more or less arbitrary names, typically alphanumeric (e.g. `x` or `y0` or `thing`); however, we will feel free to include non-alphanumeric characters in names as we see fit, since the paucity of syntax means there’s little risk of ambiguity.

Since the only available variables are those bound by enclosing lambdas, we can also infer that there are no `let` bindings for local variables, and no globals of any sort; the lambda calculus doesn’t come with a standard library.


### Summary

In quasi-BNF, the grammar for the lambda calculus is extremely minimal:

<figure class="center">

<em>e</em> <strong>:=</strong> <code>λ</code> <em>x</em> <code>.</code> <em>e</em> <strong>|</strong> <em>e</em> <em>e</em> <strong>|</strong> <em>x</em> <strong>|</strong> (<em>e</em>)

</figure>

And finally, this table gives a side-by-side comparison of the syntax of the lambda calculus with the corresponding syntax in Haskell & JavaScript:

<table>
  <caption>Syntax of the lambda calculus, Haskell, & JavaScript</caption>
  <thead>
    <tr>
      <th scope="col"></th>
      <th scope="col">Lambda calculus</th>
      <th scope="col">Haskell</th>
      <th scope="col">JavaScript</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th scope="row">Abstraction</th>
      <td><code>λ x . y</code></td>
      <td><code>\ x -> y</code></td>
      <td><code>(x) => y</code></td>
    </tr>
    <tr>
      <th scope="row">Application</th>
      <td><code>f x</code></td>
      <td><code>f x</code></td>
      <td><code>f(x)</code></td>
    </tr>
    <tr>
      <th scope="row">Variable</th>
      <td><code>x</code></td>
      <td><code>x</code></td>
      <td><code>x</code></td>
    </tr>
  </tbody>
</table>

<aside>
  <p>Due to the lambda calculus’s terseness, I will be making free use of several notational conveniences:</p>

  <ol>
    <li><p>writing <code>λ x y . z</code> as an abbreviation of <code>λ x . λ y . z</code>.</p></li>
    <li><p>writing <code>?</code> to stand for bits we don’t know yet, as though we had an environment supporting holes.</p></li>
    <li><p>writing definitions as though we had a metalanguage.</p></li>
    <li><p>referencing definitions elsewhere as though we had globals.</p></li>
    <li><p>writing type signatures as though we had a type system, and even a typechecker, with as much polymorphism and inference as is convenient at any particular moment.</p></li>
    <li><p>using syntactic recursion as though it existed.</p></li>
    <li><p>using general recursion as though it made sense.</p></li>
    <li><p>ignoring application order, normalization, reduction, substitution, values, references, allocation, copying, space, time, entropy, and any and all other such details whenever I feel like it.</p></li>
  </ol>

  <p>By convention, I will name types in <code>TitleCase</code> and both term and (local) type variables in <code>camelCase</code>.</p>

  <p>I will try to avoid pulling rabbits from hats too wantonly, but for now, I’ll ask you to suspend disbelief; I hope to revisit and justify some of these in later posts.</p>
</aside>


## Unconditional λ

Lambdas are the only way to introduce values—they’re the only “literal” syntax in the language. We can therefore infer that the only kinds of runtime values must be closures. In an interpreter for the lambda calculus, closures might consist of the name of the introduced variable, the body of the lambda, & a map relating the names and values of any variables it closed over when constructed (again, we assume strict lexical scoping). There are no bits, bytes, words, pointers, or objects in the language’s semantics; only this runtime representation of lambdas.

Likewise, lambdas are also the only way to introduce variables—there’s no standard library, built-ins, primitives, prelude, or global environment to provide common definitions. We’re truly baking the apple pie from scratch.

All of this raises the question: how do you _do_ anything when you don’t even have `true` and `false`? Lambdas and variables don’t _do_, they merely _are_, so that leaves application. When all you have is application, everything looks like a lambda abstraction, so we’ll represent booleans using lambdas.

Of course, it’s not _just_ booleans we’re after; `true` and `false` aren’t much use without `and`, `or`, `not`, `if`, and all the rest. To be useful, our representation of booleans should therefore suffice to define these, as well. But how do you define `if` without using `if`? In a lazy language like Haskell, we might define `if` as a function something like so:

```haskell
if_ :: Bool -> a -> a -> a
if_ cond then_ else_ = if cond then then_ else else_
```

In a strict language like JavaScript, we’d instead take functions for the alternatives:

```javascript
function if_(cond, then_, else_) {
  if (cond) {
    then_();
  } else {
    else_();
  }
}
```

Both these definitions use the language’s native booleans and `if` syntax (a tactic for implementing embedded DSLs known as “meta-circularity”), and thus aren’t viable in the lambda calculus. However, they do give us a hint: in both cases we have a function taking a condition, consequence, and alternative, and using the first to select one of the latter two. In the lambda calculus, we might start by writing:

```
if = λ cond then else . ?
```

(Note: there aren’t any keywords in the lambda calculus, so there’s nothing stopping me from naming variables things like `if`, a fact which I will take free advantage of.)

We’ve introduced a definition for `if`, as a function of three parameters; now what do we do with them? The lambda calculus’s stark palette makes it easy to enumerate _all_ the things we can do with some variable `a`:

1. Ignore it, whether by simply not mentioning it at all (as in `λ a . λ b . b`), or by shadowing it with another lambda which binds the same name (as in `λ a . λ a . a`).

2. Mention it, whether on its own in the body of a lambda (as in `λ a . a` or `λ a . λ b . a`), somewhere within either side of an application (as in `λ a . λ b . a b` or `λ a . λ b . b a`), or some combination of both (as in `λ a . (λ b . a) a`).

We could for example simply return `then` or `else`:

```
if = λ cond then else . then
if = λ cond then else . else
```

But in that case the conditional isn’t conditional at all—the value in no way depends on `cond`. Clearly the body must make use of all three variables if we want it to behave like the `if`s we know and love from other languages.

Taking a step back for a moment, let’s examine the roles of `if`’s arguments. `then` and `else` are passive; we only want to use or evaluate one or the other depending on the value of `cond`. `cond`, then, is the key: it takes the active role.

Thus, in the same way that our `if_` functions in Haskell & JavaScript employed those language’s features to implement, we’re going to define `if cond then else` as the application of the condition to the other two parameters:


```
if = λ cond then else . cond then else
```

<aside>
  <p>Under standard semantics for the lambda calculus, we could simplify this definition further by “<a href="https://en.wikipedia.org/wiki/Lambda_calculus#η-reduction">η-reducing</a>” it, noting that <code>λ x . f x</code> behaves the same as <code>f</code> alone. Nevertheless, we will leave this and likely other definitions in their full, η-long forms for the sake of clarity.</p>
</aside>

This feels strangely like cheating: surely we’ve only moved the problem around. Now instead of `if` making the decision about which argument to return, we’ve deferred it to `cond`. But `if` and `cond` aren’t the same, semantically; `if` takes a boolean and two other arguments and returns one of the latter, while `cond` _is_ a boolean—albeit evidently a boolean represented as a function. Let’s make that precise by writing down `if`’s type:

```
if : Bool -> a -> a -> a
```

Notwithstanding our use of the yet-to-be-defined name `Bool` for the type of the condition, this is the same type as we gave `if_` in Haskell; that’s a good sign that we’re on the right track! It takes a `Bool` and two arguments of type `a`, and it must return one of those because that’s the only way for it to come up with the `a` that it returns. But what _is_ `Bool`?

Working backwards from the type and definition of `if`, we see that `cond` is applied to two arguments, and therefore must be a function of two parameters. Further, these are both of type `a`, and the value it returns must also be of type `a` for `if`’s type to hold. Thus, we can define the type `Bool` like so:

```
Bool = ∀ a . a -> a -> a
```

<aside>
  <p>I’m making explicit use of the for-all quantifier here to drive home the point that any particular <code>Bool</code> value must be able to be applied to <code>then</code> and <code>else</code> values of any arbitrary type <code>a</code>, defined now or in the future.</p>

  <p>By the same token, we could have written <code>if</code>’s type more explicitly as:</p>

  <pre><code>if : ∀ a . Bool -> a -> a -> a</code></pre>

  <p>Here and in future, local type variables can be assumed to be implicitly generalized in the same manner as Haskell if not otherwise quantified.</p>
</aside>

If a given `Bool` is a function of two arguments of arbitrary type, returning the same type, it must therefore select one of its arguments to return. There are only two distinguishable inhabitants of `Bool`, `true` and `false`, so we can therefore deduce that since `if` defers the selection of the result to the `Bool`, for `true` and `false` to actually differ they must make opposite selections. In other words, `true` must return the `then` parameter, while `false` must return the `else` one:


```
true, false : Bool
true  = λ then else . then
false = λ then else . else
```

We didn’t move the problem around after all; we solved it. What we noticed was a deeper insight: this encoding of booleans makes `if` redundant, since if we can apply `if` to a `Bool` and two arguments, we could equally apply the `Bool` to those arguments directly.

<aside>
  <p>We chose to define <code>if</code> as applying the <code>Bool</code> to the other arguments in the same order it received them, but we could just as easily have swapped them:</p>

  <pre><code>if = λ cond then else . cond else then</code></pre>

  <p>In this case, <code>if</code> would be more useful since it would preserve our familiar argument ordering. As an exercise for the reader, consider what other effects this difference would have. What are the tradeoffs, syntactically and semantically? When would one or the other definition be more or less convenient?</p>
</aside>

It’s frequently convenient to conflate booleans with bits, their minimal representation, but in truth they’re not the same at all. Practically, some programming languages define booleans as a byte in memory, perhaps clamping its values to 0 and 1; others define them as instances of some boolean class, or constructors of an algebraic datatype. Some provide no formal relationship between `true` and `false` at all, save for a common interface—duck typing.

Mathematically, booleans are the values in propositional logic; the upper and lower bounds of a lattice; the zero and one of a semiring; the members of the set with cardinality 2; and many other things in many different contexts.

Operationally, booleans represent choice, and this is a pattern that we’ll see repeated: _encoding_ a datatype with lambdas means _representing_ the datatype as _functions supporting all of its operations_. All operations on booleans can be defined by selecting between two alternatives, which is precisely what our encoding does.

We can demonstrate this by defining some other operations on booleans, e.g. logical operators, using the encoding we’ve built thus far.

`not` takes a single `Bool` and returns another:

```
not : Bool -> Bool
not = λ x . ?
```

As when defining `if`, all we can do with a `Bool` is branch on it:

```
not = λ x . if x ? ?
```

<aside>
  <p>As discussed in a previous aside, <code>if</code> is operationally redundant—i.e. <code>if x y z</code> is operationally equivalent to <code>x y z</code>—given the ordering of arguments to <code>Bool</code>s which we selected earlier. It is, however, pleasantly evocative, and so is used for clarity and so we can stop talking about that ordering decision.</p>
</aside>

But which arguments should we pass if we wish to return a `Bool` with the opposite value? Recall the definition of `Bool` from above:

```
Bool = ∀ a . a -> a -> a
```

To return a `Bool`, therefore, each argument must likewise be a `Bool`. The first argument will be selected if `x` is `true`, the second if `x` is `false`, so if we want the opposite value from `x` we can simply apply it to the opposite values in either position:

```
not = λ x . if x false true
```

`not x` will therefore return `false` if `x` is `true`, and `true` if `x` is `false`; equationally:

```
not true  = false
not false = true
```

Which is precisely the meaning we intended `not` to have.

<aside>
  <p>Note that this is not the only way that we could have implemented <code>not</code>.</p>

  <p><code>not</code>’s type is <code>Bool -> Bool</code>, which is equivalent to <code>(∀ a . a -> a -> a) -> ∀ a . a -> a -> a</code> Thus, we could also define <code>not</code> by taking the extra arguments that the <em>result</em> <code>Bool</code> will be applied to, and using them directly, though in the opposite order:</p>

  <pre><code>not = λ x then else . if x else then</code></pre>

  <p>Or equivalently, but perhaps <em>slightly</em> more familiar:</p>

  <pre><code>not = λ x . λ then else . if x else then</code></pre>

  <p>This style of definition can be surprising if you’re not used to so-called “curried functions” as commonly used in e.g. Haskell, but it’s operationally equivalent to the definition developed above. As an exercise, try to work out why that equivalence holds.</p>
</aside>

`or` and `and` are closely related to one another, so we’ll define them simultaneously. Both take two `Bool`s and return a `Bool`:

```
or, and : Bool -> Bool -> Bool
or  = λ x y . ?
and = λ x y . ?
```

As with `not`, all we can do with `Bool`s is branch:

```
or  = λ x y . if x ? ?
and = λ x y . if x ? ?
```

For `or`, if `x` is `true`, we can return `true` immediately (“short-circuiting”). For `and`, it’s the opposite:

```
or  = λ x y . if x true ?
and = λ x y . if x ?    false
```

If `x` is `false`, `or` needs to test whether `y` is `true`; likewise, if `x` is `true`, `and` needs to test whether `y` is also `true`. Once more, all we can do with `Bool`s is branch:

```
or  = λ x y . if x true       (if y ? ?)
and = λ x y . if x (if y ? ?) false
```

And since we must return a `Bool`, we can use `true` and `false`:

```
or  = λ x y . if x true              (if y true false)
and = λ x y . if x (if y true false) false
```

Pleasantly, `if y true false` (and likewise `y true false`) is operationally equivalent to `y`. Using that equivalence, we can simplify these definitions, leaving us with:

```
or  = λ x y . if x true y
and = λ x y . if x y    false
```

<aside>
  <p>As an exercise, define <code>xor : Bool -> Bool -> Bool</code>.</p>
</aside>


## Conclusion

In this post, we’ve explored defining a ubiquitous programming language feature—booleans—using nothing more than the spartan trappings of the lambda calculus. We’ve emerged with a language which can express not merely functions and their applications, but also fundamental metaphysical concepts such as truth.

In the next post, we’ll look at lambda-encodings of beauty: ML/Haskell-style algebraic datatypes.
