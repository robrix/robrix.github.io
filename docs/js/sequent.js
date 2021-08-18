export class SeqVar extends HTMLElement {
  constructor() {
    super();
  }
  connectedCallback() {
    const shadow = loadTemplateElement(this, document.getElementById('seq-var'));
    if (this.hasAttribute('neg')) {
      shadow.getElementById('polarity').textContent = '−';
    }
    else if (this.hasAttribute('pos')) {
      shadow.getElementById('polarity').textContent = '+';
    }
  }
}

customElements.define("seq-var", SeqVar);

export class SeqOp extends HTMLElement {
  constructor(template) { super(); this.template = template; }
  connectedCallback() {
    loadTemplateElement(this, this.template, root => {
      root.getElementById('op').textContent = this.getAttribute('name');
    });
  }
}

customElements.define("seq-op", SeqOp);

export class SeqInfix extends SeqOp {
  constructor() { super(document.getElementById('seq-infix')); }
}

customElements.define("seq-infix", SeqInfix);

export class SeqNullfix extends SeqOp {
  constructor() { super(document.getElementById('seq-nullfix')); }
}

customElements.define("seq-nullfix", SeqNullfix);

export class SeqPrefix extends SeqOp {
  constructor() { super(document.getElementById('seq-prefix')); }
}

customElements.define("seq-prefix", SeqPrefix);

export class SeqInference extends HTMLElement {
  constructor() { super(); }
  connectedCallback() { loadTemplateElement(this, document.getElementById('seq-rule')); }
}

customElements.define("seq-inference", SeqInference);


export class SeqSequent extends HTMLElement {
  constructor() { super(); }
  connectedCallback() { loadTemplateElement(this, document.getElementById('seq-sequent')); }
}

customElements.define("seq-sequent", SeqSequent);

export class SeqContext extends HTMLElement {
  constructor(template) { super(); this.template = template; }
  connectedCallback() { loadTemplateElement(this, this.template, root => { root.getElementById('metavar').textContent = this.getAttribute('name'); }); }
}

customElements.define("seq-context", SeqContext);

export class SeqGamma extends SeqContext {
  constructor() { super(document.getElementById('seq-gamma')); }
  connectedCallback() { this.setAttribute('name', 'Γ'); super.connectedCallback(); }
}

customElements.define("seq-gamma", SeqGamma);

export class SeqDelta extends SeqContext {
  constructor() { super(document.getElementById('seq-delta')); }
  connectedCallback() { this.setAttribute('name', 'Δ'); super.connectedCallback(); }
}

customElements.define("seq-delta", SeqDelta);

export class SeqFocus extends HTMLElement {
  constructor() { super(); }
  connectedCallback() { loadTemplateElement(this, document.getElementById('seq-focus')); }
}

customElements.define("seq-focus", SeqFocus);


function loadTemplateElement(node, templateElement, setup) {
  const shadow = node.attachShadow({ mode: 'open' });
  const root = templateElement.content.cloneNode(true);
  if (typeof setup === "function") {
    setup(root);
  }
  shadow.appendChild(root);
  return shadow;
}
