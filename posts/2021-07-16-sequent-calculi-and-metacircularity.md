---
layout: post
title:  "Sequent Calculi and Metacircularity"
date:   2021-07-16 16:06:12
categories: logic, plt
---

Sequent calculi are powerful and flexible tools for studying logic and, via Curry-Howard, computation. But why, and how? Where does this power come from?

<!--more-->

We enjoy a variety of idioms to describe the relationship between problems and solutions. For example: “use the right tool for the job,” and “a good impedance match.” Where sequent calculi offer a good impedance match, it may in part be because of how they model the logical primitives and principles we build atop them.

A sequent is, in general, a pair of contexts (collections of propositions), often represented by the variables Γ (the antecedents, or hypotheses; computationally, the inputs) and Δ (the succedents, or consequents; computationally, the outputs), separated by the ⊢ symbol (called “the turnstile”, but pronounced “proves,” “entails,” etc.; computationally, “produces,” “returns,” etc.), where Γ is treated conjunctively and Δ disjunctively. Thus, the general sequent form:

> Γ ⊢ Δ

can be read as “all of Γ prove some of Δ,” or in computational terms, “all of Γ produce some of Δ.”

Specific configurations of sequent have more precise interpretations as a consequence of these general rules, and these give us examples of how we use the sequent calculus to build systems for logic and computation.

Starting simply, we have truth:

> · ⊢ A

(NB: · on either side of the turnstile means an empty context.) Reading this literally, “nothing proves A,” but “nothing is required to prove A” or “A is provable without any extra information” are clearer. Or, simply, “A is true.” Dually, falsehood:

> A ⊢ ·

Literally, “A proves nothing,” or “nothing is derivable from A;” more simply, “A is false.”

These two examples show us the edge cases of the two contexts: since we interpret Γ conjunctively, the empty case is truth; and since we interpret Δ disjunctively, the empty case is falsehood. The corner case, where both is empty, is interesting too:

> · ⊢ ·

Interpreting as we did for truth and falsehood, “truth proves falsity”—a contradiction. This is a surprisingly useful tool to have logically, and perhaps even more so computationally, tho that will have to wait for a future post.

Truth and falsehood are the units of conjunction and disjunction, respectively, and since we treat the contexts in such manner it should be no surprise that we get those behaviours directly:

> A, B ⊢ ·

Straightforwardly, “A and B are false.” Note the “and” there: conjunction! On the other side of the turnstile:

> · ⊢ A, B

“A or B is true.” This is how we get disjunction.

This time, the opposite corner case isn’t much more interesting than the edges it intersects:

> A, B ⊢ C, D

“A and B prove C or D” is a perfectly cromulent sequent to have kicking around, but we don’t learn a lot more from its treatment of the contexts. But observe that the turnstile plays a role here as well. It’s not _just_ punctuation, or rather, it is, but the space it punctuates is important. “A proves B” is (more or less) another way of saying “A implies B,” and implication is the last piece missing. (Well, there’s negation, but you can compose that from the pieces we have so far.)

To recap, we’ve seen how to model ⊤ (truth), ⊥ (falsity), ∨ (disjunction), ∧ (conjunction), → (implication), and finally, contradiction. All of that just from examining individual sequents, not even considering how they’re composed together! Furthermore, you can make a case that the rules for how we treat occurrences of variables in sequents directly allow the encoding of universal and existential quantification.

This is perhaps my favourite example of metacircular interpretation; for another example, the ideal language to implement a lisp in turns out to be a lisp, because it’s already got everything a lisp needs.
