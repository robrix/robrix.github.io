---
layout: post
title:  "Pattern matching over recursive values in Swift"
date:   2015-07-01 02:29:44
categories: swift
---

Swift’s value types are _almost_ able to represent algebraic data types. Unfortunately, they fall short of the mark  when it comes to recursion, and while they’ve announced that  their solution, indirect `case`s, will ship in a later build of Swift 2, there’s still reason to want them today.

The standard solution is to use `Box<T>`, a function, or some other reference type to manually force an indirection for the recursive cases:

{% highlight swift %}
enum Expression {
	case Variable(String)
	case Abstraction(String, Box<Expression>)
	case Application(Box<Expression>, Box<Expression>)
}
{% endhighlight %}

Unfortunately, this has a few significant warts:

- Clients of the API have to know about `Box<T>`; it can’t be a private implementation detail. This can in turn lead to ambiguities if APIs aren’t using a common dependency to provide the `Box<T>` type. Further, they have to box and unbox the values themselves.
- Pattern matching cannot be performed recursively in a single step.

Indirect cases will (I believe) resolve both of these issues, but there’s another solution we can apply today which solves both _and_ provides a significant increase in the expressiveness of the type, at the expense of introducing a (useful) intermediary type.

To begin with, note that in Swift 2, it’s no longer necessary to box elements of parameterized types in enum cases. This suggests a straightforward refactoring: replace `Expression`’s recursive instances with elements of a type parameter:

{% highlight swift %}
enum Expression<Recur> {
	case Variable(String)
	case Abstraction(String, Recur)
	case Application(Recur, Recur)
}
{% endhighlight %}

Now we’ve got an `Expression` type that can be instantiated with a given type parameter to recur. But if we try to describe the type of a recursive instance of it, we immediately run into a wall:

{% highlight swift %}
let expression: Expression<Expression<Expression<…>>>
{% endhighlight %}

It would seem we’ve simply moved the problem from the `case`s to the type, and can now see more clearly why Swift doesn’t allow `case`s to recur directly: it amounts to an infinite type. Some indirection is required, _somewhere_, and by allowing the programmer to place it (whether by explicit boxing or an `indirect` keyword), the performance implications are somewhat under their control, rather than the compiler’s.

We need some way to tie `Expression` into a knot (as it were), looping back around into itself, but without requiring us to write out an infinite list of nested type parameters. If we were writing a function instead of a type, we could use the [`fix`](https://github.com/robrix/Prelude#fix) function, which computes the [least fixed point](https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed_point_combinator) of a function, to lift a nonrecursive function into a recursive one:

{% highlight swift %}
let factorial = fix { recur in
    { n in n > 0 ? n * recur(n - 1) : 1 }
}
{% endhighlight %}

Instead of making a recursive function, we make a nonrecursive function taking a function as a parameter, and return an inner function which calls through it in order to recur. `fix` calls the outer function with a closure which calls back into `fix`, tying the knot. Is there an analogous fixed point for types? If there were, we would expect it to have the same overall shape: it would apply a type constructor like `Expression<T>` to a type which itself provides the connection back to `Expression<T>`.

I’ll let you in on a secret: types are functions, too. `Expression<T>` is actually a function, abstracted over a parameter `T` to a concrete instance of `Expression` with `Recur` instantiated to `T`. And it turns out that, like other functions, types also have fixed points.

In Haskell (the inevitable destination of any discussion of fixed points in programming languages), we could write this `Fix` type of a parameter type `f` like so:

{% highlight haskell %}
data Fix f = Fix (f (Fix f))
{% endhighlight %}

This is Haskell notation approaching its densest form, so let’s compare it with how `fix` (the least fixed point of functions) is defined in Swift:

{% highlight swift %}
public func fix<A, B>(f: (A -> B) -> A -> B) -> A -> B {
	return { f(fix(f))($0) }
}
{% endhighlight %}

The `fix` function applies `f`, the function parameter passed to `fix`, to the result of applying `fix` recursively to `f` again. It wraps this up in another closure to avoid infinite looping.

Analogously, the `Fix` type applies `f`, the type parameter passed to `fix`, to the result of applying `Fix` recursively to `f` again. Haskell is lazily-evaluated, so it doesn’t need to wrap the lot of it up again.

Let’s try writing `Fix` in Swift. It only has one `case`, so it can be a struct instead of an `enum`.

{% highlight swift %}
struct Fix {}
{% endhighlight %}

Now it needs to have a type parameter, `F`.

{% highlight swift %}
struct Fix<F> {}
{% endhighlight %}

So far so good. Now we need to apply `F` to itself, recursively. But doesn’t that cause the infinite sequence of nested types again? `Fix<F<Fix<F<…>>>>` is no improvement on `Expression<Expression<Expression<…>>>`.

Fortunately, Swift allows you to refer to a type without reference to its type parameters in its body:

{% highlight swift %}
struct Fix<F> {
	let body: F<Fix>
}
{% endhighlight %}

Unfortunately, while `Fix` is a complete reference inside the body of this type, Swift doesn’t know that `F` can accept type parameters, and thus rejects this. We can be sneaky and use a `protocol` with a `typealias` to work around this:

{% highlight swift %}
protocol Fixable {
	typealias Recur
}

struct Fix<F: Fixable> {
	let body: F
}
{% endhighlight %}

But now when we add the constraint to tie `F` into a knot, we run into a new issue: `swiftc` crashes. ([rdar://20000145](http://www.openradar.appspot.com/20000145)).

{% highlight swift %}
protocol Fixable {
	typealias Recur
}

struct Fix<F: Fixable where F.Recur == Fix> {
	let body: F
}
// => fish: Job 1, 'swift boom.swift' terminated by signal SIGSEGV (Address boundary error)
{% endhighlight %}

Fortunately, while Swift can’t express a _generic_ `Fix` over any arbitrary fixable type, it _can_ express a fixed point of `Expression` _specifically_. Let’s call this new type `Term`. Once again, it’s a `struct`, and its body holds an `Expression` instantiated to itself. This one errors out, but it’s clear we’re getting closer:

{% highlight swift %}
struct Term {
	let body: Expression<Term>
}
// => error: recursive value type 'Term' is not allowed
{% endhighlight %}

`Term` is recursive because it holds an `Expression` which in turn holds (in some of its cases) a `Recur`, which we’ve instantiated to `Term`. We need to reintroduce an indirection via a reference type like `Box<T>` or a function.

Haven’t we just moved the problem around again? Well, sort of. Certainly we still need to box the values, but now we can do it in one and only one place—`Term`—and additionally we can make it `private`, avoiding exposing our implementation details to our consumers. Our constructor and getter can handle the boxing and unboxing for us:

{% highlight swift %}
struct Term {
	init(body: Expression<Term>) {
		boxedBody = Box(body)
	}
	
	var body: Expression<Term> {
		return boxedBody.value
	}
	
	private let boxedBody: Box<Expression<Term>>
}
{% endhighlight %}

That’s a pretty decent reason to use this approach right now (if you can’t wait for indirect cases). But it only solves one of the problems we mentioned initially; we still can’t pattern match recursively. For example, if we wanted to evaluate application expressions, we would want to write something like this:

{% highlight swift %}
switch expression {
case let .Application(.Abstraction(variable, body), argument):
	// substitute argument for variable in body
default:
	// throw an error
}
{% endhighlight %}

But because of the `Term` and `Box`, neither of which can be matched through, we would have to write this instead:

{% highlight swift %}
switch expression {
case let .Application(abstraction, argument):
	switch abstraction.body {
	case let .Abstraction(variable, body):
		// substitute argument for variable in body
	default:
		break
	}
	fallthrough
default:
	// throw an error
}
{% endhighlight %}

If we could flatten out the type, we could pattern match. Flattening out the type would put us straight back into the infinite sequence of `Expression<…>`s; but maybe we can only _partially_ flatten it?

We don’t need to pattern match against arbitrarily-nested terms for this example; we just want to match against a single nested layer. Therefore, we really only need to flatten out a single step of the recursive type. We’d need to apply this for each appearance of `Recur` in `Expression`, replacing it with `Expression<Recur>`.

Replacing each instance of a type parameter with an instance of another type parameter sounds like a job for a `map` function. In Haskell, this function is known as `fmap`,  for functor map, where functors are a kind of mathematical object with some specific shape, and where map preserves this shape. For example, the `Array.map` method, given some function `transform`, produces a new array with the same number of elements and in the same order (i.e. preserving the structure of the array), but with each element replaced by applying `transform`. Array, then, is a functor; and it turns out, so is our `Expression` tree.

In our case, `map` should replace the `Recur` instances with the result of applying some function to them. There are no instances of `Recur` in `Variable` cases, so it should just re-wrap the variable name in the resulting type; the `Abstraction` and `Application` cases will apply `transform`:

{% highlight swift %}
enum Expression<Recur> {
	…
	func map<Other>(transform: Recur -> Other) -> Expression<Other> {
		switch self {
		case let .Variable(x):
			return .Variable(x)
		case let .Abstraction(x, body):
			return .Abstraction(x, transform(body))
		case let .Application(a, b):
			return .Application(transform(a), transform(b))
		}
	}
}
{% endhighlight %}

We can use this to implement [recursion schemes](http://patrickthomson.ghost.io/an-introduction-to-recursion-schemes/), improving our confidence in recursive functions over the type, but for now we’ll limit ourselves to enabling pattern matching. Given an `Expression<Recur>`, we want to replace each `Recur` with its recursive instantiation, `Expression<Recur>`. Otherwise put, we need a function of type `Expression<Recur> -> Expression<Expression<Recur>>`. Let’s implement this as a method, and call it destructure (since it decomposes the structure of the type):

{% highlight swift %}
enum Expression<Recur> {
	…
	func destructure() -> Expression<Expression<Recur>> {
		return map {
			// what do we do here?
		}
	}
}
{% endhighlight %}

…but we can’t! To implement a function of type `Expression<Recur> -> Expression<Expression<Recur>>` using `map`, we’d need a function of type `Recur -> Expression<Recur>` to pass to it. There is no useful function that can do this; without knowing a _specific_ (and actually recursive) type for `Recur`, we have no way to recover the `Expression<Recur>` that we want to return.

Instead, let’s use a constrained extension to limit ourselves to `Expression<Term>`. Unfortunately it’s not _quite_ that simple, because Swift, for reasons beyond my knowledge ([rdar://21512469](http://www.openradar.appspot.com/21512469)), forbids the obvious thing:

{% highlight swift %}
extension Expression where Recur == Term { … }
// => error: same-type requirement makes generic parameter 'Recur' non-generic
{% endhighlight %}

We’ll work around this using a protocol, `FixpointType`:

{% highlight swift %}
protocol FixpointType {
	typealias Fixed
}

extension Term: FixpointType {
	typealias Fixed = Expression<Term>
}
{% endhighlight %}

Now we can constrain the extension to `FixpointType` like we want:

{% highlight swift %}
extension Expression where Recur : FixpointType, Recur.Fixed == Expression<Recur> {
	func destructure() -> Expression<Expression<Recur>> {
		return map {
			// what do we do here?
		}
	}
}
{% endhighlight %}

There are two problems remaining with this implementation:

1. We still don’t have a way to get an `Expression<Recur>` from a `Recur`.
 2. `swiftc` crashes. ([rdar://21328632](http://openradar.appspot.com/21328632))

Fortunately, we can resolve the former by adding a property to the protocol:

{% highlight swift %}
protocol FixpointType {
	typealias Fixed
	var body: Fixed { get }
}
{% endhighlight %}

With that out of the way, we can work around the crash by loosening the constraints slightly; we don’t actually require that `Recur.Fixed` be recursive; we just need to be able to name it. Now we can give the return type of `destructure` as `Expression<Recur.Fixed>`, and implement it in the obvious way, mapping each term to its body:

{% highlight swift %}
extension Expression where Recur : FixpointType {
	func destructure() -> Expression<Recur.Fixed> {
		return map { term in term.body }
	}
}
{% endhighlight %}

Now we can use `destructure` to implement evaluation of well-formed `.Application` expressions, using exactly the pattern matching we wanted in the first place:

{% highlight swift %}
switch expression.destructure() {
case let .Application(.Abstraction(variable, body), argument):
	// substitute argument for variable in body
default:
	// throw an error
}
{% endhighlight %}

[Full code listing](https://gist.github.com/robrix/4696ec3c117aac97ef0b).