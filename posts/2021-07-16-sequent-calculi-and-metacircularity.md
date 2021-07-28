---
layout: post
title:  "Sequent Calculi and Metacircularity"
date:   2021-07-16 16:06:12
categories: logic, plt
---

<style type="text/css">
table.rule.left td {
  text-align: right;
}
table.rule.right td {
  text-align: left;
}
</style>

(Some) sequent calculi offer a few simple primitives organized around the eponymous _sequent_, comprised of two contexts—collections of zero or more propositions—named Γ (comprising the antecedents, or hypotheses) and Δ (comprising the consequents), separated by the ⊢ symbol, called “the turnstile” and pronounced variously as “proves,” “supports,” “entails,” and so forth. Interpreted computationally, ⊢ can also be understood as “constructs” or “returns.” Put together, we write:

> Γ ⊢ Δ

<!--more-->

or, instantiating the contexts with a proposition A:

> A ⊢ A

Propositions are automatically coerced to contexts, and the comma operator serves as concatenation on contexts, so we can also have multiple propositions/contexts interleaved however we like:

> Γ<sub>1</sub>, A, Γ<sub>2</sub> ⊢ Δ<sub>1</sub>, A, Δ<sub>2</sub>

Γ is interpreted conjunctively and Δ disjunctively, so we can read the above as “all of Γ<sub>1</sub>, A, and Γ<sub>2</sub> prove some of Δ<sub>1</sub>, A, Δ<sub>2</sub>”, or more generally, “all of Γ proves some of Δ.”

When the contexts are empty, they’re interpreted as the identity elements of their respective interpretations, i.e. an empty Γ is interpreted the same as a true proposition, and an empty Δ the same as a false one.

Combined, these properties offer straightforward means to discuss logical constants and operators:

- Δ behaves like ⊥ when empty.
- Δ behaves like ∨ when non-empty.
- Γ behaves like ⊤ when empty.
- Γ behaves like ∧ when non-empty.
- ⊢ behaves like →.
- When both contexts are empty, ⊢ on its own is a contradiction. (Intuitively, this is because the empty Γ is truth and the empty Δ is falsity—“truth proves falsity.”)

This is reflected in the rules for various logical operators.

<table class="connective">
  <tbody>
    <tr>
      <td>
        <table class="rule left">
          <tbody>
            <tr>
              <td rowspan="2">
                <p>∨ ⊢</p>
              </td>
              <td class="premise">
                <p>A, Γ ⊢ Δ</p>
              </td>
              <td class="premise">
                <p>B, Γ ⊢ Δ</p>
              </td>
            </tr>
            <tr>
              <td colspan="2">
                <p>A ∨ B, Γ ⊢ Δ</p>
              </td>
            </tr>
          </tbody>
        </table>
      </td>
      <td>
        <table class="rule right">
          <tbody>
            <tr>
              <td>
                <p>Γ ⊢ Δ, A</p>
              </td>
              <td rowspan="2">
                <p>⊢ ∨<sub>1</sub></p>
              </td>
            </tr>
            <tr>
              <td>
                <p>Γ ⊢ Δ, A ∨ B</p>
              </td>
            </tr>
          </tbody>
        </table>
        <table class="rule right">
          <tbody>
            <tr>
              <td>
                <p>Γ ⊢ Δ, B</p>
              </td>
              <td rowspan="2">
                <p>⊢ ∨<sub>2</sub></p>
              </td>
            </tr>
            <tr>
              <td>
                <p>Γ ⊢ Δ, A ∨ B</p>
              </td>
            </tr>
          </tbody>
        </table>
      </td>
    </tr>
  </tbody>
</table>

<table class="connective">
  <tbody>
    <tr>
      <td>
        <table class="rule left">
          <tbody>
            <tr>
              <td rowspan="2">
                <p>∧ ⊢<sub>1</sub></p>
              </td>
              <td class="premise">
                <p>A, Γ ⊢ Δ</p>
              </td>
            </tr>
            <tr>
              <td class="conclusion">
                <p>A ∧ B, Γ ⊢ Δ</p>
              </td>
            </tr>
          </tbody>
        </table>
        <table class="rule left">
          <tbody>
            <tr>
              <td rowspan="2">
                <p>∧ ⊢<sub>2</sub></p>
              </td>
              <td class="premise">
                <p>B, Γ ⊢ Δ</p>
              </td>
            </tr>
            <tr>
              <td class="conclusion">
                <p>A ∧ B, Γ ⊢ Δ</p>
              </td>
            </tr>
          </tbody>
        </table>
      </td>
      <td>
        <table class="rule right">
          <tbody>
            <tr>
              <td class="premise">
                <p>Γ ⊢ Δ, A</p>
              </td>
              <td class="premise">
                <p>Γ ⊢ Δ, B</p>
              </td>
              <td rowspan="2">
                <p>⊢ ∧</p>
              </td>
            </tr>
            <tr>
              <td colspan="2">
                <p>Γ ⊢ Δ, A ∧ B</p>
              </td>
            </tr>
          </tbody>
        </table>
      </td>
    </tr>
  </tbody>
</table>
