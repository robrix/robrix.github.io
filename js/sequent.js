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


export const SeqSymbol = component('seq-symbol')`
<style type="text/css">
var {
  font-style: normal;
  font-size: 1.25rem;
  line-height: 1.8rem;
  font-weight: 100;
  font-family: var(--sequent-font);
}
:host([neg]) {
  color: var(--sequent-neg-colour);
}
:host([pos]) {
  color: var(--sequent-pos-colour);
}
</style><var><slot></slot></var>
`;


// operators

export class SeqInfix extends HTMLElement {
  constructor(symbol, polarity) {
    super();
    this.shadowNode = shadow(this)`
<slot name="lhs"></slot> <seq-symbol ${polarity}>${symbol}</seq-symbol> <slot name="rhs"></slot><slot id="slot"></slot>
    `;
  }
  connectedCallback() {
    const [lhs, rhs] = this.shadowRoot.getElementById('slot').assignedElements();
    lhs.slot = 'lhs';
    rhs.slot = 'rhs';
  }
}

export function infix(tag, symbol, polarity, setup) {
  const klass = class extends SeqInfix {
    constructor() {
      super(symbol, polarity);
    }
  };
  if (typeof setup === 'function') {
    setup(klass);
  }
  customElements.define(tag, klass);
  return klass;
}


export class SeqOp extends HTMLElement {
  constructor(opName, polarity) {
    super();
    this.shadowNode = shadow(this)`
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
  width: auto;
}
</style>
<slot id="lhs" name="left"></slot>
<span><seq-symbol id="op"></seq-symbol><slot name="op-decoration"></slot></span>
<slot></slot>
<slot id="rhs" name="right"></slot>
    `;
    this.shadowNode.getElementById('op').textContent = opName || this.getAttribute('name');
    if (polarity === 'neg' || polarity === 'pos') {
      this.setAttribute(polarity, '');
    }
  }
}
customElements.define('seq-op', SeqOp);


// inferences

export const SeqInference = component('seq-inference', SeqInference => {
  SeqInference.prototype.connectedCallback = function () {
    const labelVar = this.shadowNode.getElementById('label-var');
    labelVar.textContent = this.getAttribute('name');
    if (this.hasAttribute('neg')) {
      labelVar.setAttribute('neg', '');
    }
    else if (this.hasAttribute('pos')) {
      labelVar.setAttribute('pos', '');
    }
  };
})`
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
#sequents {
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
  <div id="label"><slot name="label"><seq-symbol id="label-var"></seq-symbol><slot name="label-decoration"></slot></slot></div>
  <div id="sequents">
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
  <span class="turnstile">⊢<slot name="turnstile-decoration"></slot></span>
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


export function template(templateSources, ...rest) {
  const template = document.createElement('template');
  template.innerHTML = rest.reduce((accum, each, ix) => { accum.push(each); accum.push(templateSources[ix + 1]); return accum; }, [templateSources[0]]).join('').trim();
  return template;
}

export function shadow(node) {
  return (templateSource, ...rest) => {
    const shadow = node.attachShadow({ mode: 'open' });
    shadow.appendChild(template(templateSource, ...rest).content.cloneNode(true));
    return shadow;
  }
}

export function component(tag, setup) {
  return (templateSource, ...rest) => {
    const klass = class extends HTMLElement {
      constructor() {
        super();
        this.shadowNode = shadow(this)(templateSource, ...rest);
      }
    };
    if (typeof setup === 'function') {
      setup(klass);
    }
    customElements.define(tag, klass);
    return klass;
  };
}

export function readwriteAttr(attr) {
  return {
    get() {
      return this.getAttribute(attr);
    },
    set(x) {
      this.setAttribute(attr, x);
    }
  };
}
