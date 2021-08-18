// vars

export class SeqVar extends HTMLElement {
  connectedCallback() {
    const template = document.createElement('template');
    template.innerHTML = `
<style type="text/css">
var {
  font-style: italic;
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
sup {
  font-style: normal;
  font-size: 60%;
  line-height: 0;
}
</style><var><slot></slot><sup id="polarity"></sup></var>
    `.trim();
    const shadow = loadTemplate(this, template);
    if (this.hasAttribute('neg')) {
      shadow.getElementById('polarity').textContent = '−';
    }
    else if (this.hasAttribute('pos')) {
      shadow.getElementById('polarity').textContent = '+';
    }
  }
}

customElements.define("seq-var", SeqVar);


// operators

export class SeqOp extends HTMLElement {
  connectedCallback() {
    loadTemplate(this, this.template, root => {
      root.getElementById('op').textContent = this.getAttribute('name');
    });
  }
}

customElements.define("seq-op", SeqOp);

export class SeqInfix extends SeqOp {
  connectedCallback() {
    this.template = document.createElement('template');
    this.template.innerHTML = `
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
    `.trim();
    super.connectedCallback();
  }
}

customElements.define("seq-infix", SeqInfix);

export class SeqNullfix extends SeqOp {
  connectedCallback() {
    this.template = document.createElement('template');
    this.template.innerHTML = `
<style type="text/css">
:host([neg]) #op {
  color: var(--sequent-neg-colour);
}
:host([pos]) #op {
  color: var(--sequent-pos-colour);
}
</style><span id="op"></span>
    `.trim();
    super.connectedCallback();
  }
}

customElements.define("seq-nullfix", SeqNullfix);

export class SeqPrefix extends SeqOp {
  connectedCallback() {
    this.template = document.createElement('template');
    this.template.innerHTML = `
<style type="text/css">
:host([neg]) #op {
  color: var(--sequent-neg-colour);
}
:host([pos]) #op {
  color: var(--sequent-pos-colour);
}
</style><span id="op"></span><slot></slot>
    `.trim();
    super.connectedCallback();
  }
}

customElements.define("seq-prefix", SeqPrefix);


// inferences

export class SeqInference extends HTMLElement {
  connectedCallback() {
    const template = document.createElement('template');
    template.innerHTML = `
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
    `.trim();
    loadTemplate(this, template);
  }
}

customElements.define("seq-inference", SeqInference);


// sequents

export class SeqSequent extends HTMLElement {
  connectedCallback() {
    const template = document.createElement('template');
    template.innerHTML = `
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
    `.trim();
    loadTemplate(this, template);
  }
}

customElements.define("seq-sequent", SeqSequent);


// contexts

export class SeqGamma extends HTMLElement {
  connectedCallback() {
    const template = document.createElement('template');
    template.innerHTML = `
<style type="text/css">
var {
  font-style: italic;
}
</style><slot></slot><var>Γ</var>
    `.trim();
    loadTemplate(this, template)
  }
}

customElements.define("seq-gamma", SeqGamma);

export class SeqDelta extends HTMLElement {
  connectedCallback() {
    const template = document.createElement('template');
    template.innerHTML = `
<style type="text/css">
var {
  font-style: italic;
}
</style><var>Δ</var><slot></slot>
    `.trim();
    loadTemplate(this, template);
  }
}

customElements.define("seq-delta", SeqDelta);


// foci

export class SeqFocus extends HTMLElement {
  connectedCallback() { shadow(this)`[<slot></slot>]`; }
}

customElements.define("seq-focus", SeqFocus);


function template(templateSource) {
  const template = document.createElement('template');
  template.innerHTML = templateSource.join();
  return template;
}

function shadow(node) {
  return templateSource => {
    const templateElement = template(templateSource);
    const shadow = node.attachShadow({ mode: 'open' });
    const root = templateElement.content.cloneNode(true);
    shadow.appendChild(root);
    return shadow;
  }
}

function loadTemplate(node, templateElement, setup) {
  const shadow = node.attachShadow({ mode: 'open' });
  const root = templateElement.content.cloneNode(true);
  if (typeof setup === "function") {
    setup(root);
  }
  shadow.appendChild(root);
  return shadow;
}
