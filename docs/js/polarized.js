import { component, infix, prefix } from './sequent.js';

export const SeqWith = infix('seq-with', '&amp;', 'neg');
export const SeqSum = infix('seq-sum', '⊕', 'pos');
export const SeqTop = prefix('seq-top', '⊤', 'neg');
export const SeqZero = prefix('seq-zero', '0', 'pos');
export const SeqPar = infix('seq-par', '⅋', 'neg');
export const SeqTensor = infix('seq-tensor', '⊗', 'pos');
export const SeqBottom = prefix('seq-bottom', '⊥', 'neg', c => {
  c.prototype.connectedCallback = function () {
    const sub = document.createElement('sub');
    sub.textContent = 'R';
    sub.className = 'neg';
    this.appendChild(sub);
    sub.slot = 'op-decoration';
  };
});
export const SeqOne = prefix('seq-one', '1', 'pos', c => {
  c.prototype.connectedCallback = function () {
    const sub = document.createElement('sub');
    sub.textContent = 'E';
    sub.className = 'pos';
    this.appendChild(sub);
    sub.slot = 'op-decoration';
  };
});
export const SeqImplication = infix('seq-impl', '→', 'neg');
export const SeqCoimplication = infix('seq-coimpl', '⤚', 'pos');
export const SeqNot = prefix('seq-not', '¬', 'neg');
export const SeqNegate = prefix('seq-negate', '∼', 'pos');
export const SeqNotUntrue = prefix('seq-not-untrue', '¬̷', 'neg');
export const SeqTrue = prefix('seq-true', '✓', 'pos');
export const SeqUpShift = prefix('seq-up', '↑', 'neg');
export const SeqDownShift = prefix('seq-down', '↓', 'pos');
export const SeqForAll = prefix('seq-forall', '∀', 'neg');
export const SeqExists = prefix('seq-exists', '∃', 'pos');
