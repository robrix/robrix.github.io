export class SeqVar extends HTMLElement {
  constructor() {
    super();
  }
  connectedCallback() {
    const shadow = loadTemplate(this, "seq-var");
    if (this.hasAttribute('neg')) {
      shadow.getElementById('polarity').textContent = '−';
    }
    else if (this.hasAttribute('pos')) {
      shadow.getElementById('polarity').textContent = '+';
    }
  }
}

customElements.define("seq-var", SeqVar);

export class SeqConn extends HTMLElement {
  constructor() {
    super();
  }
  connectedCallback() {
    const conn = loadTemplate(this, "seq-conn", conn => {
      conn.getElementById('op').textContent = this.getAttribute('name');
    });
  }
}

customElements.define("seq-conn", SeqConn);

export class SeqInference extends HTMLElement {
  constructor() {
    super();
  }
  connectedCallback() { loadTemplate(this, "seq-rule"); }
}

customElements.define("seq-inference", SeqInference);


export class SeqSequent extends HTMLElement {
  constructor() { super(); }
  connectedCallback() { loadTemplate(this, "seq-sequent"); }
}

customElements.define("seq-sequent", SeqSequent);

export class SeqFocus extends HTMLElement {
  constructor() { super(); }
  connectedCallback() { loadTemplate(this, "seq-focus"); }
}

customElements.define("seq-focus", SeqFocus);

function loadTemplate(node, templateName, setup) {
  const shadow = node.attachShadow({ mode: 'open' });
  const root = document.getElementById(templateName).content.cloneNode(true);
  if (typeof setup === "function") {
    setup(root);
  }
  shadow.appendChild(root);
  return shadow;
}
