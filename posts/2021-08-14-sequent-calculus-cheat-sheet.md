---
measure: measure-wide
title:  "Sequent calculus cheat sheet"
date:   2<span class="pos">0</span>21-<span class="pos">0</span>8-14 23:<span class="pos">0</span>1:<span class="pos">0</span>2
categories: logic, plt
---

Rules for sequent calculus connectives, formatted as a cheat sheet.

<!--more-->

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
            <div class="rule">
              <div class="label"><span class="neg">&amp;</span>⊢<sub>1</sub></div>
              <div class="inference">
                <div class="premise Γ">[<span class="var neg">A</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span></div>
                <div class="premise turnstile">⊢</div>
                <div class="premise Δ"><span class="context Δ">Δ</span></div>
                <span class="line-of-inference"></span>
                <div class="conclusion Γ">[<span class="var neg">A</span><sup class="neg">−</sup> <span class="neg">&amp;</span> <span class="var neg">B</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span></div>
                <div class="conclusion turnstile">⊢</div>
                <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
              </div>
            </div>
            <div class="rule">
              <div class="label"><span class="neg">&amp;</span>⊢<sub>2</sub></div>
              <div class="inference">
                <div class="premise Γ">[<span class="var neg">B</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span></div>
                <div class="premise turnstile">⊢</div>
                <div class="premise Δ"><span class="context Δ">Δ</span></div>
                <span class="line-of-inference"></span>
                <div class="conclusion Γ">[<span class="var neg">A</span><sup class="neg">−</sup> <span class="neg">&amp;</span> <span class="var neg">B</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span></div>
                <div class="conclusion turnstile">⊢</div>
                <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
              </div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span>, <span class="var neg">A</span><sup class="neg">−</sup></div>
                <div class="premise"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span>, <span class="var neg">B</span><sup class="neg">−</sup></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span>, <span class="var neg">A</span><sup class="neg">−</sup> <span class="neg">&amp;</span> <span class="var neg">B</span><sup class="neg">−</sup></div>
            </div>
            <div class="label">⊢<span class="neg">&amp;</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="pos">~</span>(<span class="var neg">A</span><sup class="neg">−</sup> <span class="neg">&amp;</span> <span class="var neg">B</span><sup class="neg">−</sup>) ≈ <span class="pos">~</span><span class="var neg">A</span><sup class="neg">−</sup> <span class="pos">⊕</span> <span class="pos">~</span><span class="var neg">B</span><sup class="neg">−</sup></p>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="pos">⊕</span>⊢</div>
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
                <div class="premise"><span class="var pos">B</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="var pos">A</span><sup class="pos">+</sup> <span class="pos">⊕</span> <span class="var pos">B</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="column">
            <div class="rule">
              <div class="inference">
                <div class="premise Γ"><span class="context Γ">Γ</span></div>
                <div class="premise turnstile">⊢</div>
                <div class="premise Δ"><span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup>]</div>
                <span class="line-of-inference"></span>
                <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
                <div class="conclusion turnstile">⊢</div>
                <div class="conclusion Δ"><span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup> <span class="pos">⊕</span> <span class="var pos">B</span><sup class="pos">+</sup>]</div>
              </div>
              <div class="label">⊢<span class="pos">⊕</span><sub>1</sub></div>
            </div>
            <div class="rule">
              <div class="inference">
                <div class="premise Γ"><span class="context Γ">Γ</span></div>
                <div class="premise turnstile">⊢</div>
                <div class="premise Δ"><span class="context Δ">Δ</span> [<span class="var pos">B</span><sup class="pos">+</sup>]</div>
                <span class="line-of-inference"></span>
                <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
                <div class="conclusion turnstile">⊢</div>
                <div class="conclusion Δ"><span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup> <span class="pos">⊕</span> <span class="var pos">B</span><sup class="pos">+</sup>]</div>
              </div>
              <div class="label">⊢<span class="pos">⊕</span><sub>2</sub></div>
            </div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="neg">¬</span>(<span class="var pos">A</span><sup class="pos">+</sup> <span class="pos">⊕</span> <span class="var pos">B</span><sup class="pos">+</sup>) ≈ <span class="pos">~</span><span class="var pos">A</span><sup class="pos">+</sup> <span class="neg">&amp;</span> <span class="pos">~</span><span class="var pos">B</span><sup class="pos">+</sup></p>
            </div>
          </div>
        </div>
      </td>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule">
            <p>no rule for <span class="neg">⊤</span>⊢</p>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="axiom"></div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span>, <span class="neg">⊤</span></div>
            </div>
            <div class="label">⊢<span class="neg">⊤</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="pos">~</span><span class="neg">⊤</span> ≈ <span class="pos">0</span></p>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="pos">0</span>⊢</div>
            <div class="inference">
              <div class="axiom"></div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="pos">0</span>, <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <p>no rule for ⊢<span class="pos">0</span></p>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="pos">~</span><span class="neg">⊤</span> ≈ <span class="pos">0</span></p>
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
          <div class="rule">
            <div class="label"><span class="neg">⅋</span>⊢</div>
            <div class="inference">
              <div class="premises">
                <div class="premise">[<span class="var neg">A</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
                <div class="premise">[<span class="var neg">B</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion">[<span class="var neg">A</span><sup class="neg">−</sup> <span class="neg">⅋</span> <span class="var neg">B</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span>, <span class="var neg">A</span><sup class="neg">−</sup>, <span class="var neg">B</span><sup class="neg">−</sup></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span>, <span class="var neg">A</span><sup class="neg">−</sup> <span class="neg">⅋</span> <span class="var neg">B</span><sup class="neg">−</sup></div>
            </div>
            <div class="label">⊢<span class="neg">⅋</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="pos">~</span>(<span class="var neg">A</span><sup class="neg">−</sup> <span class="neg">⅋</span> <span class="var neg">B</span><sup class="neg">−</sup>) ≈ <span class="pos">~</span><span class="var neg">A</span><sup class="neg">−</sup> <span class="pos">⊗</span> <span class="pos">~</span><span class="var neg">B</span><sup class="neg">−</sup></p>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="pos">⊗</span>⊢</div>
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A</span><sup class="pos">+</sup>, <span class="var pos">B</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="var pos">A</span><sup class="pos">+</sup> <span class="pos">⊗</span> <span class="var pos">B</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup>]</div>
                <div class="premise"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span> [<span class="var pos">B</span><sup class="pos">+</sup>]</div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup> <span class="pos">⊗</span> <span class="var pos">B</span><sup class="pos">+</sup>]</div>
            </div>
            <div class="label">⊢<span class="pos">⊗</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="neg">¬</span>(<span class="var pos">A</span><sup class="pos">+</sup> <span class="pos">⊗</span> <span class="var pos">B</span><sup class="pos">+</sup>) ≈ <span class="pos">~</span><span class="var pos">A</span><sup class="pos">+</sup> <span class="neg">⅋</span> <span class="pos">~</span><span class="var pos">B</span><sup class="pos">+</sup></p>
            </div>
          </div>
        </div>
      </td>
    </tr>
    <tr>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="neg">⊥</span><sub class="R">R</sub>⊢<sub class="R">R</sub></div>
            <div class="inference">
              <div class="axiom"></div>
              <span class="line-of-inference"></span>
              <div class="conclusion">[<span class="neg">⊥</span><sub class="R">R</sub>] <span class="context Γ">Γ</span> ⊢<sub class="R">R</sub> <span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢<sub class="R">R</sub></div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢<sub class="R">R</sub></div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span>, <span class="neg">⊥</span><sub class="R">R</sub></div>
            </div>
            <div class="label">⊢<span class="neg">⊥</span><sub class="R">R</sub></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="pos">~</span><span class="neg">⊥</span><sub class="R">R</sub> ≈ <span class="pos">1</span><sub class="E">E</sub></p>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="pos">1</span><sub class="E">E</sub>⊢<sub class="E">E</sub></div>
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢<sub class="E">E</sub></div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="pos">1</span><sub class="E">E</sub>, <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢<sub class="E">E</sub></div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="axiom"></div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="context Γ">Γ</span> ⊢<sub class="E">E</sub> <span class="context Δ">Δ</span> [<span class="pos">1</span><sub class="E">E</sub>]</div>
            </div>
            <div class="label">⊢<sub class="E">E</sub><span class="pos">1</span><sub class="E">E</sub></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="neg">¬</span><span class="pos">1</span><sub class="E">E</sub> ≈ <span class="neg">⊥</span><sub class="R">R</sub></p>
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
          <div class="rule">
            <div class="label"><span class="neg">→</span>⊢</div>
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup>]</div>
                <div class="premise">[<span class="var neg">B</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion">[<span class="var pos">A</span><sup class="pos">+</sup> <span class="neg">→</span> <span class="var neg">B</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span>, <span class="var neg">B</span><sup class="neg">−</sup></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span>, <span class="var pos">A</span><sup class="pos">+</sup> <span class="neg">→</span> <span class="var neg">B</span><sup class="neg">−</sup></div>
            </div>
            <div class="label">⊢<span class="neg">→</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="pos">~</span>(<span class="var pos">A</span><sup class="pos">+</sup> <span class="neg">→</span> <span class="var neg">B</span><sup class="neg">−</sup>) ≈ <span class="var neg">B</span><sup class="neg">−</sup> <span class="pos">⤚</span> <span class="var pos">A</span><sup class="pos">+</sup></p>
            </div>
            <div class="rule">
              <p><span class="var pos">A</span><sup class="pos">+</sup> <span class="neg">→</span> <span class="var neg">B</span><sup class="neg">−</sup> ≈ <span class="neg">¬</span><span class="var pos">A</span><sup class="pos">+</sup> <span class="neg">⅋</span> <span class="var neg">B</span><sup class="neg">−</sup></p>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="pos">⤚</span>⊢</div>
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span>, <span class="var neg">B</span><sup class="neg">−</sup></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="var neg">B</span><sup class="neg">−</sup> <span class="pos">⤚</span> <span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup>]</div>
                <div class="premise">[<span class="var neg">B</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span> [<span class="var neg">B</span><sup class="neg">−</sup> <span class="pos">⤚</span> <span class="var pos">A</span><sup class="pos">+</sup>]</div>
            </div>
            <div class="label">⊢<span class="pos">⤚</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="neg">¬</span>(<span class="var neg">B</span><sup class="neg">−</sup> <span class="pos">⤚</span> <span class="var pos">A</span><sup class="pos">+</sup>) ≈ <span class="var pos">A</span><sup class="pos">+</sup> <span class="neg">→</span> <span class="var neg">B</span><sup class="neg">−</sup></p>
            </div>
            <div class="rule">
              <p><span class="var neg">B</span><sup class="neg">−</sup> <span class="pos">⤚</span> <span class="var pos">A</span><sup class="pos">+</sup> ≈ <span class="pos">~</span><span class="var pos">A</span><sup class="pos">+</sup> <span class="pos">⊗</span> <span class="var neg">B</span><sup class="neg">−</sup></p>
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
          <div class="rule">
            <div class="label"><span class="neg">¬</span>⊢</div>
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup>]</div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ">[<span class="neg">¬</span><span class="var pos">A</span><sup class="pos">+</sup>] <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span>, <span class="neg">¬</span><span class="var pos">A</span><sup class="pos">+</sup></div>
            </div>
            <div class="label">⊢<span class="neg">¬</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="pos">~</span><span class="neg">¬</span><span class="var pos">A</span><sup class="pos">+</sup> ≈ <span class="var pos">A</span><sup class="pos">+</sup></p>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="pos">~</span>⊢</div>
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span>, <span class="var neg">A</span><sup class="neg">−</sup></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="pos">~</span><span class="var neg">A</span><sup class="neg">−</sup>, <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ">[<span class="var neg">A</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span> [<span class="pos">~</span><span class="var neg">A</span><sup class="neg">−</sup>]</div>
            </div>
            <div class="label">⊢<span class="pos">~</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="neg">¬</span><span class="pos">~</span><span class="var neg">A</span><sup class="neg">−</sup> ≈ <span class="var neg">A</span><sup class="neg">−</sup></p>
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
          <div class="rule">
            <div class="label"><span class="neg">¬̷</span>⊢</div>
            <div class="inference">
              <div class="premise Γ">[<span class="var neg">A</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ">[<span class="neg">¬̷</span><span class="var neg">A</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span>, <span class="var neg">A</span><sup class="neg">−</sup></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span>, <span class="neg">¬̷</span><span class="var neg">A</span><sup class="neg">−</sup></div>
            </div>
            <div class="label">⊢<span class="neg">¬̷</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="pos">~</span><span class="neg">¬̷</span><span class="var neg">A</span><sup class="neg">−</sup> ≈ <span class="var neg">A</span><sup class="neg">−</sup></p>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="pos">✓</span>⊢</div>
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="pos">✓</span><span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup>]</div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span> [<span class="pos">✓</span><span class="var pos">A</span><sup class="pos">+</sup>]</div>
            </div>
            <div class="label">⊢<span class="pos">✓</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="neg">¬</span><span class="pos">✓</span><span class="var pos">A</span><sup class="pos">+</sup> ≈ <span class="var pos">A</span><sup class="pos">+</sup></p>
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
          <div class="rule">
            <div class="label"><span class="neg">↑</span>⊢</div>
            <div class="inference">
              <div class="premise Γ"><span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ">[<span class="neg">↑</span><span class="var pos">A</span><sup class="pos">+</sup>] <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup>]</div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span>, <span class="neg">↑</span><span class="var pos">A</span><sup class="pos">+</sup></div>
            </div>
            <div class="label">⊢<span class="neg">↑</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p>(<span class="var">A</span> <span class="neg">→</span> <span class="var neg">B</span><sup class="neg">−</sup>)<sup class="neg">N</sup> = <span class="pos">↓</span>(<span class="var">A</span><sup class="neg">N</sup>) <span class="neg">→</span> <span class="var neg">B</span><sup class="neg">−</sup></p>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="pos">↓</span>⊢</div>
            <div class="inference">
              <div class="premise Γ">[<span class="var neg">A</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="pos">↓</span><span class="var neg">A</span><sup class="neg">−</sup>, <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span>, <span class="var neg">A</span><sup class="neg">−</sup></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span> [<span class="pos">↓</span><span class="var neg">A</span><sup class="neg">−</sup>]</div>
            </div>
            <div class="label">⊢<span class="pos">↓</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p>(<span class="var">A</span> <span class="neg">→</span> <span class="var">B</span>)<sup class="pos">V</sup> = <span class="pos">↓</span>(<span class="var">A</span><sup class="pos">V</sup> <span class="neg">→</span> <span class="neg">↑</span>(<span class="var">B</span><sup class="pos">V</sup>))</p>
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
          <div class="rule">
            <div class="label"><span class="neg">∀</span>⊢</div>
            <div class="inference">
              <div class="premise Γ">[<span class="var neg">A</span><sup class="neg">−</sup>{<span class="var">B</span>/<span class="var">X</span>}] <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ">[<span class="neg">∀</span><span class="var">X</span>.<span class="var neg">A</span><sup class="neg">−</sup>] <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule right">
            <div class="inference">
              <div class="premises">
                <div class="premise"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span>, <span class="var neg">A</span><sup class="neg">−</sup></div>
                <div class="side-condition"><span class="var">X</span> ∉ <span class="function">fv</span>(<span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span>)</div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span>, <span class="neg">∀</span><span class="var">X</span>.<span class="var neg">A</span><sup class="neg">−</sup></div>
            </div>
            <div class="label">⊢<span class="neg">∀</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="pos">~</span>(<span class="neg">∀</span><span class="var">X</span>.<span class="var neg">A</span><sup class="neg">−</sup>) ≈ <span class="pos">∃</span><span class="var">X</span>.<span class="pos">~</span><span class="var neg">A</span><sup class="neg">−</sup></p>
            </div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule left">
            <div class="label"><span class="pos">∃</span>⊢</div>
            <div class="inference">
              <div class="premises">
                <div class="side-condition"><span class="var">X</span> ∉ <span class="function">fv</span>(<span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span>)</div>
                <div class="premise"><span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="pos">∃</span><span class="var">X</span>.<span class="var pos">A</span><sup class="pos">+</sup>, <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span> [<span class="var pos">A</span><sup class="pos">+</sup>{<span class="var">B</span>/<span class="var">X</span>}]</div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span> [<span class="pos">∃</span><span class="var">X</span>.<span class="var pos">A</span><sup class="pos">+</sup>]</div>
            </div>
            <div class="label">⊢<span class="pos">∃</span></div>
          </div>
          <div class="connective">
            <div class="rule">
              <p><span class="neg">¬</span>(<span class="pos">∃</span><span class="var">X</span>.<span class="var pos">A</span><sup class="pos">+</sup>) ≈ <span class="neg">∀</span><span class="var">X</span>.<span class="neg">¬</span><span class="var pos">A</span><sup class="pos">+</sup></p>
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
              <div class="conclusion turnstile">⊢</div>
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
                <div class="premise"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span>, <span class="var">A</span></div>
                <div class="premise"><span class="var">A</span>, <span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
              </div>
              <span class="line-of-inference"></span>
              <div class="conclusion"><span class="context Γ">Γ</span> ⊢ <span class="context Δ">Δ</span></div>
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
          <div class="rule">
            <div class="label"><span class="function">weaken</span> ⊢</div>
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="var">A</span>, <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span>, <span class="var">A</span></div>
            </div>
            <div class="label">⊢ <span class="function">weaken</span></div>
          </div>
        </div>
      </td>
      <td>
        <div class="connective">
          <div class="rule">
            <div class="label"><span class="function">contract</span> ⊢</div>
            <div class="inference">
              <div class="premise Γ"><span class="var">A</span>, <span class="var">A</span>, <span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="var">A</span>, <span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span></div>
            </div>
          </div>
          <div class="rule">
            <div class="inference">
              <div class="premise Γ"><span class="context Γ">Γ</span></div>
              <div class="premise turnstile">⊢</div>
              <div class="premise Δ"><span class="context Δ">Δ</span>, <span class="var">A</span>, <span class="var">A</span></div>
              <span class="line-of-inference"></span>
              <div class="conclusion Γ"><span class="context Γ">Γ</span></div>
              <div class="conclusion turnstile">⊢</div>
              <div class="conclusion Δ"><span class="context Δ">Δ</span>, <span class="var">A</span></div>
            </div>
            <div class="label">⊢ <span class="function">contract</span></div>
          </div>
        </div>
      </td>
    </tr>
  </tbody>
</table>
