// vars

export const SeqVar = component('seq-var', SeqVar => {
  Object.defineProperty(SeqVar.prototype, 'polarity', {
    get() {
      return this.hasAttribute('neg') ? '−' : this.hasAttribute('pos') ? '+' : null;
    }
  })
  SeqVar.prototype.connectedCallback = function () {
    this.shadowNode.getElementById('polarity').textContent = this.polarity;
  };
})`
<style type="text/css">
var {
  font-style: italic;
  font-size: 1.25rem;
  line-height: 1.8rem;
  font-weight: 100;
  font-family: var(--sequent-font);
}
sup {
  font-style: normal;
  font-size: 60%;
  line-height: 0;
}
</style><var><slot></slot><sup id="polarity"></sup></var>
`;


// operators

export const SeqInfix = component('seq-infix', SeqInfix => {
  Object.defineProperty(SeqInfix.prototype, 'name', readwriteAttr('name'));
  SeqInfix.prototype.connectedCallback = function () {
    this.shadowNode.getElementById('op').textContent = this.name;
  };
})`
<style type="text/css">
:host([neg]) #op {
  color: var(--sequent-neg-colour);
}
:host([pos]) #op {
  color: var(--sequent-pos-colour);
}
:host {
  display: inline-flex;
  flex-direction: row;
  gap: 6px;
  width: auto;
}
</style><slot name="left"></slot>
<span id="op"></span>
<slot name="right"></slot>
`;

export const SeqNullfix = component('seq-nullfix', SeqNullfix => {
  Object.defineProperty(SeqNullfix.prototype, 'name', readwriteAttr('name'));
  SeqNullfix.prototype.connectedCallback = function () {
    this.shadowNode.getElementById('op').textContent = this.name;
  };
})`
<style type="text/css">
:host([neg]) #op {
  color: var(--sequent-neg-colour);
}
:host([pos]) #op {
  color: var(--sequent-pos-colour);
}
</style><span id="op"></span>
`;

export const SeqPrefix = component('seq-prefix', SeqPrefix => {
  Object.defineProperty(SeqPrefix.prototype, 'name', readwriteAttr('name'));
  SeqPrefix.prototype.connectedCallback = function () {
    this.shadowNode.getElementById('op').textContent = this.name;
  };
})`
<style type="text/css">
:host([neg]) #op {
  color: var(--sequent-neg-colour);
}
:host([pos]) #op {
  color: var(--sequent-pos-colour);
}
</style><span id="op"></span><slot></slot>
`;


// inferences

export const SeqInference = component('seq-inference')`
<style type="text/css">
#rule {
  display: flex;
  flex-direction: row;
  gap: 2px;
  width: max-content;
  align-items: center;
  font-family: var(--sequent-font);
}
:host([left]) #rule {
  flex-direction: row;
}
:host([right]) #rule {
  flex-direction: row-reverse;
}
#label {
  font-size: 75%;
  padding: 6px;
}
#inference {
  font-size: 1.25rem;
  line-height: 1.8rem;
  font-weight: 100;
  display: grid;
  grid-template-columns: auto max-content auto;
  gap: 2px 0;
  width: max-content;
}
#line-of-inference {
  grid-column: 1 / -1;
  background: #666;
  height: 1px;
}
div.axiom {
  grid-column: 1 / -1;
  height: 1.75rem;
  line-height: 1.8rem;
  text-align: center;
}
div.axiom::after {
  content: "∎"
}
</style>
<div id="rule">
  <div id="label"><slot name="label"></slot></div>
  <div id="inference">
    <slot name="premise"><div class="axiom"></div></slot>
    <span id="line-of-inference"></span>
    <slot name="conclusion"></slot>
  </div>
</div>
`;


// sequents

export const SeqSequent = component('seq-sequent')`
<style type="text/css">
.sequent {
  display: grid;
  grid-template-columns: auto max-content auto;
  gap: 2px 0;
  width: max-content;
  font-family: var(--sequent-font);
}
:host([slot]),
:host([slot]) .sequent {
  display: contents;
}
#left, .turnstile, #right {
  padding: 2px 4px;
}
#left {
  text-align: right;
}
.turnstile {
  text-align: center;
}
#right {
  text-align: left;
}
</style>
<div class="sequent">
  <span id="left"><slot name="left"></slot></span>
  <span class="turnstile">⊢</span>
  <span id="right"><slot name="right"></slot></span>
</div>
`;


// contexts

export const SeqGamma = component('seq-gamma')`
<style type="text/css">
var {
  font-style: italic;
}
</style><slot></slot><var>Γ</var>
`;

export const SeqDelta = component('seq-delta')`
<style type="text/css">
var {
  font-style: italic;
}
</style><var>Δ</var><slot></slot>
`;


// foci

export const SeqFocus = component('seq-focus')`[<slot></slot>]`;


function template(templateSource) {
  const template = document.createElement('template');
  template.innerHTML = templateSource.join().trim();
  return template;
}

function shadow(node) {
  return templateSource => {
    const shadow = node.attachShadow({ mode: 'open' });
    shadow.appendChild(template(templateSource).content.cloneNode(true));
    return shadow;
  }
}

function component(tag, setup) {
  return templateSource => {
    const klass = class extends HTMLElement {
      constructor() {
        super();
        this.shadowNode = shadow(this)(templateSource);
      }
    };
    if (typeof setup === 'function') {
      setup(klass);
    }
    customElements.define(tag, klass);
    return klass;
  };
}

function readwriteAttr(attr) {
  return {
    get() {
      return this.getAttribute(attr);
    },
    set(x) {
      this.setAttribute(attr, x);
    }
  };
}
