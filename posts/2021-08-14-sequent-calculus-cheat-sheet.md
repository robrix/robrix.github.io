---
measure: measure-medium
title:  "Sequent calculus cheat sheet"
date:   2<span class="zero">0</span>21-<span class="zero">0</span>8-14 23:<span class="zero">0</span>1:<span class="zero">0</span>2
categories: logic, plt
---

Rules for sequent calculus connectives, formatted as a cheat sheet.

<!--more-->

<link rel="stylesheet" type="text/css" href="https://cdn.rawgit.com/dreampulse/computer-modern-web-font/master/fonts.css">

<table class="borderless">
  <colgroup>
    <col class="negative">
    <col class="positive">
  </colgroup>
  <thead>
    <tr>
      <th class="neg">-</th>
      <th class="pos">+</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th colspan="2">
        <h3 id="additive">Additive</h3>
      </th>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="column">
            <div class="rule left">
              <div class="label"><span class="with op">&amp;</span><sub>1</sub></div>
              <div class="inference">
                <div class="premise Γ"><span class="focus">[<span class="var neg">A<sup>−</sup></span>]</span> <span class="Γ">Γ</span></div>
                <div class="premise turnstile"><span class="turnstile">⊢</span></div>
                <div class="premise Δ"><span class="Δ">Δ</span></div>
                <span class="line-of-inference"></span>
                <div class="conclusion Γ"><span class="focus">[<span class="with"><span class="var neg">A<sup>−</sup></span> <span class="op">&amp;</span> <span class="var neg">B<sup>−</sup></span></span>]</span> <span class="Γ">Γ</span></div>
                <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
                <div class="conclusion Δ"><span class="Δ">Δ</span></div>
              </div>
            </div>
            <div class="rule left">
              <div class="label"><span class="with op">&amp;</span><sub>2</sub></div>
              <div class="inference">
                <div class="premise Γ"><span class="focus">[<span class="var neg">B<sup>−</sup></span>]</span> <span class="Γ">Γ</span></div>
                <div class="premise turnstile"><span class="turnstile">⊢</span></div>
                <div class="premise Δ"><span class="Δ">Δ</span></div>
                <span class="line-of-inference"></span>
                <div class="conclusion Γ"><span class="focus">[<span class="with"><span class="var neg">A<sup>−</sup></span> <span class="op">&amp;</span> <span class="var neg">B<sup>−</sup></span></span>]</span> <span class="Γ">Γ</span></div>
                <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
                <div class="conclusion Δ"><span class="Δ">Δ</span></div>
              </div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span>, <span class="var neg">A<sup>−</sup></span></div>
                <div class="premise"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span>, <span class="var neg">B<sup>−</sup></span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span>, <span class="with"><span class="var neg">A<sup>−</sup></span> <span class="op">&amp;</span> <span class="var neg">B<sup>−</sup></span></span></div>
            </div>
            <div class="label"><span class="with op">&amp;</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="negate"><span class="op">∼</span>(<span class="with"><span class="var neg">A<sup>−</sup></span> <span class="with op">&amp;</span> <span class="var neg">B<sup>−</sup></span></span>)</span> ≈ <span class="negate"><span class="op">∼</span></span><span class="var neg">A<sup>−</sup></span> <span class="sum op">⊕</span> <span class="negate"><span class="op">∼</span></span><span class="var neg">B<sup>−</sup></span>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="sum op">⊕</span></div>
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="var pos">A<sup>+</sup></span>, <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
                <div class="premise"><span class="var pos">B<sup>+</sup></span>, <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="sum"><span class="var pos">A<sup>+</sup></span> <span class="op">⊕</span> <span class="var pos">B<sup>+</sup></span></span>, <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="column">
            <div class="rule right">
              <div class="inference">
                <div class="premise Γ"><span class="Γ">Γ</span></div>
                <div class="premise turnstile"><span class="turnstile">⊢</span></div>
                <div class="premise Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="var pos">A<sup>+</sup></span>]</span></div>
                <span class="line-of-inference"></span>
                <div class="conclusion Γ"><span class="Γ">Γ</span></div>
                <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
                <div class="conclusion Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="sum"><span class="var pos">A<sup>+</sup></span> <span class="op">⊕</span> <span class="var pos">B<sup>+</sup></span></span>]</span></div>
              </div>
              <div class="label"><span class="sum op">⊕</span><sub>1</sub></div>
            </div>
            <div class="rule right">
              <div class="inference">
                <div class="premise Γ"><span class="Γ">Γ</span></div>
                <div class="premise turnstile"><span class="turnstile">⊢</span></div>
                <div class="premise Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="var pos">B<sup>+</sup></span>]</span></div>
                <span class="line-of-inference"></span>
                <div class="conclusion Γ"><span class="Γ">Γ</span></div>
                <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
                <div class="conclusion Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="sum"><span class="var pos">A<sup>+</sup></span> <span class="op">⊕</span> <span class="var pos">B<sup>+</sup></span></span>]</span></div>
              </div>
              <div class="label"><span class="sum op">⊕</span><sub>2</sub></div>
            </div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="not"><span class="op">¬</span>(<span class="sum"><span class="var pos">A<sup>+</sup></span> <span class="op">⊕</span> <span class="var pos">B<sup>+</sup></span></span>)</span> ≈ <span class="with"><span class="negate"><span class="op">∼</span><span class="var pos">A<sup>+</sup></span></span> <span class="op">&amp;</span> <span class="negate"><span class="op">∼</span><span class="var pos">B<sup>+</sup></span></span></span>
            </div>
          </div>
        </div>
      </td>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule left">
            <p>no left rule for <span class="top">⊤</span></p>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="axiom"></div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span>, <span class="top">⊤</span></div>
            </div>
            <div class="label"><span class="top">⊤</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="negate"><span class="op">∼</span><span class="top">⊤</span></span> ≈ <span class="zero">0</span>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="zero">0</span></div>
            <div class="inference">
              <div class="axiom"></div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="zero">0</span>, <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <p>no right rule for <span class="zero">0</span></p>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="not"><span class="op">¬</span><span class="zero">0</span></span> ≈ <span class="top">⊤</span>
            </div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <th colspan="2">
        <h3 id="multiplicative">Multiplicative</h3>
      </th>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="par op">⅋</span></div>
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="focus">[<span class="var neg">A<sup>−</sup></span>]</span> <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
                <div class="premise"><span class="focus">[<span class="var neg">B<sup>−</sup></span>]</span> <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="focus">[<span class="par"><span class="var neg">A<sup>−</sup></span> <span class="op">⅋</span> <span class="var neg">B<sup>−</sup></span></span>]</span> <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span>, <span class="var neg">A<sup>−</sup></span>, <span class="var neg">B<sup>−</sup></span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span>, <span class="par"><span class="var neg">A<sup>−</sup></span> <span class="op">⅋</span> <span class="var neg">B<sup>−</sup></span></span></div>
            </div>
            <div class="label"><span class="par op">⅋</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="negate"><span class="op">∼</span>(<span class="par"><span class="var neg">A<sup>−</sup></span> <span class="op">⅋</span> <span class="var neg">B<sup>−</sup></span></span>)</span> ≈ <span class="tensor"><span class="negate"><span class="op">∼</span></span><span class="var neg">A<sup>−</sup></span> <span class="op">⊗</span> <span class="negate"><span class="op">∼</span></span><span class="var neg">B<sup>−</sup></span></span>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="tensor op">⊗</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A<sup>+</sup></span>, <span class="var pos">B<sup>+</sup></span>, <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="tensor"><span class="var pos">A<sup>+</sup></span> <span class="op">⊗</span> <span class="var pos">B<sup>+</sup></span></span>, <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span> <span class="focus">[<span class="var pos">A<sup>+</sup></span>]</span></div>
                <div class="premise"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span> <span class="focus">[<span class="var pos">B<sup>+</sup></span>]</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span> <span class="focus">[<span class="tensor"><span class="var pos">A<sup>+</sup></span> <span class="op">⊗</span> <span class="var pos">B<sup>+</sup></span></span>]</span></div>
            </div>
            <div class="label"><span class="tensor op">⊗</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="not"><span class="op">¬</span></span>(<span class="tensor"><span class="var pos">A<sup>+</sup></span> <span class="op">⊗</span> <span class="var pos">B<sup>+</sup></span></span>) ≈ <span class="par"><span class="not op">∼</span><span class="var pos">A<sup>+</sup></span> <span class="op">⅋</span> <span class="not op">∼</span><span class="var pos">B<sup>+</sup></span></span>
            </div>
          </div>
        </div>
      </td>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="bottom op R">⊥</span></div>
            <div class="inference">
              <div class="axiom"></div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="focus">[<span class="bottom op R">⊥</span>]</span> <span class="Γ">Γ</span> <span class="turnstile R">⊢</span> <span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile R">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile R">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span>, <span class="bottom op R">⊥</span></div>
            </div>
            <div class="label"><span class="bottom op R">⊥</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="negate"><span class="op">∼</span><span class="bottom op R">⊥</span></span> ≈ <span class="one op E">1</span>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="one op E">1</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile E">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="one op E">1</span>, <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile E">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="axiom"></div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="Γ">Γ</span> <span class="turnstile E">⊢</span> <span class="Δ">Δ</span> <span class="focus">[<span class="one op E">1</span>]</span></div>
            </div>
            <div class="label"><span class="one op E">1</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="not"><span class="op">¬</span></span><span class="one op E">1</span> ≈ <span class="bottom op R">⊥</span>
            </div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <th colspan="2">
        <h3 id="implicative">Implicative</h3>
      </th>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="implication op">→</span></div>
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span> <span class="focus">[<span class="var pos">A<sup>+</sup></span>]</span></div>
                <div class="premise"><span class="focus">[<span class="var neg">B<sup>−</sup></span>]</span> <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="focus">[<span class="implication"><span class="var pos">A<sup>+</sup></span> <span class="op">→</span> <span class="var neg">B<sup>−</sup></span></span>]</span> <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A<sup>+</sup></span>, <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span>, <span class="var neg">B<sup>−</sup></span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span>, <span class="implication"><span class="var pos">A<sup>+</sup></span> <span class="op">→</span> <span class="var neg">B<sup>−</sup></span></span></div>
            </div>
            <div class="label"><span class="implication op">→</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="negate"><span class="op">∼</span></span>(<span class="implication"><span class="var pos">A<sup>+</sup></span> <span class="op">→</span> <span class="var neg">B<sup>−</sup></span></span>) ≈ <span class="coimplication"><span class="var neg">B<sup>−</sup></span> <span class="op">⤚</span> <span class="var pos">A<sup>+</sup></span></span>
            </div>
            <div class="equivalence">
              <span class="implication"><span class="var pos">A<sup>+</sup></span> <span class="op">→</span> <span class="var neg">B<sup>−</sup></span></span> ≈ <span class="not"><span class="op">¬</span><span class="var pos">A<sup>+</sup></span></span> <span class="par op">⅋</span> <span class="var neg">B<sup>−</sup></span>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="coimplication op">⤚</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A<sup>+</sup></span>, <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span>, <span class="var neg">B<sup>−</sup></span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="coimplication"><span class="var neg">B<sup>−</sup></span> <span class="op">⤚</span> <span class="var pos">A<sup>+</sup></span></span>, <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span> <span class="focus">[<span class="var pos">A<sup>+</sup></span>]</span></div>
                <div class="premise"><span class="focus">[<span class="var neg">B<sup>−</sup></span>]</span> <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span> <span class="focus">[<span class="coimplication"><span class="var neg">B<sup>−</sup></span> <span class="op">⤚</span> <span class="var pos">A<sup>+</sup></span></span>]</span></div>
            </div>
            <div class="label"><span class="coimplication op">⤚</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="not"><span class="op">¬</span></span>(<span class="coimplication"><span class="var neg">B<sup>−</sup></span> <span class="op">⤚</span> <span class="var pos">A<sup>+</sup></span></span>) ≈ <span class="implication"><span class="var pos">A<sup>+</sup></span> <span class="op">→</span> <span class="var neg">B<sup>−</sup></span></span>
            </div>
            <div class="equivalence">
              <span class="coimplication"><span class="var neg">B<sup>−</sup></span> <span class="op">⤚</span> <span class="var pos">A<sup>+</sup></span></span> ≈ <span class="negate"><span class="op">∼</span></span><span class="var pos">A<sup>+</sup></span> <span class="tensor op">⊗</span> <span class="var neg">B<sup>−</sup></span>
            </div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <th colspan="2">
        <h3 id="negation">Negation</h3>
      </th>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="not op">¬</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="var pos">A<sup>+</sup></span>]</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="focus">[<span class="not"><span class="op">¬</span><span class="var pos">A<sup>+</sup></span></span>]</span> <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A<sup>+</sup></span>, <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span>, <span class="not"><span class="op">¬</span><span class="var pos">A<sup>+</sup></span></span></div>
            </div>
            <div class="label"><span class="not"><span class="op">¬</span></span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="negate"><span class="op">∼</span><span class="not"><span class="op">¬</span><span class="var pos">A<sup>+</sup></span></span></span> ≈ <span class="var pos">A<sup>+</sup></span>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="negate"><span class="op">∼</span></span></div>
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span>, <span class="var neg">A<sup>−</sup></span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="negate"><span class="op">∼</span><span class="var neg">A<sup>−</sup></span></span>, <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="focus">[<span class="var neg">A<sup>−</sup></span>]</span> <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="negate"><span class="op">∼</span><span class="var neg">A<sup>−</sup></span></span>]</span></div>
            </div>
            <div class="label"><span class="negate"><span class="op">∼</span></span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="not"><span class="op">¬</span><span class="negate"><span class="op">∼</span><span class="var neg">A<sup>−</sup></span></span></span> ≈ <span class="var neg">A<sup>−</sup></span>
            </div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <th scope="row">
        <h3 id="assertion">Assertion</h3>
      </th>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="not-untrue op">¬̷</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="focus">[<span class="var neg">A<sup>−</sup></span>]</span> <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="focus">[<span class="not-untrue"><span class="op">¬̷</span><span class="var neg">A<sup>−</sup></span></span>]</span> <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span>, <span class="var neg">A<sup>−</sup></span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span>, <span class="not-untrue"><span class="op">¬̷</span><span class="var neg">A<sup>−</sup></span></span></div>
            </div>
            <div class="label"><span class="not-untrue op">¬̷</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="negate"><span class="op">∼</span><span class="not-untrue"><span class="op">¬̷<span class="var neg">A<sup>−</sup></span></span></span></span> ≈ <span class="var neg">A<sup>−</sup></span>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="true op">✓</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A<sup>+</sup></span>, <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="true"><span class="op">✓</span><span class="var pos">A<sup>+</sup></span></span>, <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="var pos">A<sup>+</sup></span>]</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="true"><span class="op">✓</span><span class="var pos">A<sup>+</sup></span></span>]</span></div>
            </div>
            <div class="label"><span class="true op">✓</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="not"><span class="op">¬</span><span class="true"><span class="op">✓</span><span class="var pos">A<sup>+</sup></span></span></span> ≈ <span class="var pos">A<sup>+</sup></span>
            </div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <th scope="row">
        <h3 id="shifts">Shifts</h3>
      </th>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="up-shift op">↑</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A<sup>+</sup></span>, <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="focus">[<span class="up-shift"><span class="op">↑</span><span class="var pos">A<sup>+</sup></span></span>]</span> <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="var pos">A<sup>+</sup></span>]</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span>, <span class="up-shift"><span class="op">↑</span><span class="var pos">A<sup>+</sup></span></span></div>
            </div>
            <div class="label"><span class="up-shift op">↑</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              (<span class="implication"><span class="var">A</span> <span class="op">→</span> <span class="var neg">B<sup>−</sup></span></span>)<sup class="N">N</sup> = <span class="implication"><span class="down-shift"><span class="op">↓</span>(<span class="var">A</span><sup class="N">N</sup>)</span> <span class="op">→</span> <span class="var neg">B<sup>−</sup></span></span>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="down-shift op">↓</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="focus">[<span class="var neg">A<sup>−</sup></span>]</span> <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="down-shift"><span class="op">↓</span><span class="var neg">A<sup>−</sup></span></span>, <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span>, <span class="var neg">A<sup>−</sup></span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="down-shift"><span class="op">↓</span><span class="var neg">A<sup>−</sup></span></span>]</span></div>
            </div>
            <div class="label"><span class="down-shift op">↓</span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              (<span class="implication"><span class="var">A</span> <span class="op">→</span> <span class="var">B</span></span>)<sup class="V">V</sup> = <span class="implication"><span class="down-shift"><span class="op">↓</span>(<span class="var">A</span><sup class="V">V</sup> <span class="op">→</span> <span class="up-shift"><span class="op">↑</span>(<span class="var">B</span><sup class="V">V</sup>)</span>)</span></span>
            </div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <th colspan="2">
        <h3 id="quantification">Quantification</h3>
      </th>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="for-all"><span class="op">∀</span></span></div>
            <div class="inference">
              <div class="premise Γ"><span class="focus">[<span class="var neg">A<sup>−</sup></span>{<span class="var">B</span>/<span class="var">X</span>}]</span> <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="focus">[<span class="for-all"><span class="op">∀</span><span class="var">X</span>.<span class="var neg">A<sup>−</sup></span></span>]</span> <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span>, <span class="var neg">A<sup>−</sup></span></div>
                <div class="side-condition"><span class="var">X</span> ∉ <span class="function">fv</span>(<span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span>)</div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span>, <span class="for-all"><span class="op">∀</span><span class="var">X</span>.<span class="var neg">A<sup>−</sup></span></span></div>
            </div>
            <div class="label"><span class="for-all"><span class="op">∀</span></span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="negate"><span class="op">∼</span>(<span class="for-all"><span class="op">∀</span><span class="var">X</span>.<span class="var neg">A<sup>−</sup></span></span>)</span> ≈ <span class="there-exists"><span class="op">∃</span><span class="var">X</span>.<span class="negate"><span class="op">∼</span><span class="var neg">A<sup>−</sup></span></span></span>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="there-exists"><span class="op">∃</span></span></div>
            <div class="inference">
              <div class="premises">
                <div class="side-condition"><span class="var">X</span> ∉ <span class="function">fv</span>(<span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span>)</div>
                <div class="premise"><span class="var pos">A<sup>+</sup></span>, <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="there-exists"><span class="op">∃</span><span class="var">X</span>.<span class="var pos">A<sup>+</sup></span></span>, <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="var pos">A<sup>+</sup></span>{<span class="var">B</span>/<span class="var">X</span>}]</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span> <span class="focus">[<span class="there-exists"><span class="op">∃</span><span class="var">X</span>.<span class="var pos">A<sup>+</sup></span></span>]</span></div>
            </div>
            <div class="label"><span class="there-exists"><span class="op">∃</span></span></div>
          </div>
          <div class="connective">
            <div class="equivalence">
              <span class="not"><span class="op">¬</span>(<span class="there-exists"><span class="op">∃</span><span class="var">X</span>.<span class="var pos">A<sup>+</sup></span></span>)</span> ≈ <span class="for-all"><span class="op">∀</span><span class="var">X</span>.<span class="not"><span class="op">¬</span><span class="var pos">A<sup>+</sup></span></span></span>
            </div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <th scope="row">
        <h3 id="core">Core</h3>
      </th>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="inference">
              <div class="axiom"></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="var">A</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="var">A</span></div>
            </div>
            <div class="label"><span class="function">init</span></div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span>, <span class="var">A</span></div>
                <div class="premise"><span class="var">A</span>, <span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="Γ">Γ</span> <span class="turnstile">⊢</span> <span class="Δ">Δ</span></div>
            </div>
            <div class="label"><span class="function">cut</span></div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
  <tbody>
    <tr>
      <th scope="row">
        <h3 id="structural">Structural</h3>
      </th>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="function">weaken</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="var">A</span>, <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span>, <span class="var">A</span></div>
            </div>
            <div class="label"><span class="function">weaken</span></div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="function">contract</span></div>
            <div class="inference">
              <div class="premise Γ"><span class="var">A</span>, <span class="var">A</span>, <span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="var">A</span>, <span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premise Γ"><span class="Γ">Γ</span></div>
              <div class="premise turnstile"><span class="turnstile">⊢</span></div>
              <div class="premise Δ"><span class="Δ">Δ</span>, <span class="var">A</span>, <span class="var">A</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="Γ">Γ</span></div>
              <div class="conclusion turnstile"><span class="turnstile">⊢</span></div>
              <div class="conclusion Δ"><span class="Δ">Δ</span>, <span class="var">A</span></div>
            </div>
            <div class="label"><span class="function">contract</span></div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
</table>
