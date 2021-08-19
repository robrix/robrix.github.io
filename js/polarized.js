import { component, infix, prefix } from './sequent.js';

export const SeqWith = infix('seq-with', '&amp;', 'neg');
export const SeqSum = infix('seq-sum', '⊕', 'pos');
export const SeqTop = prefix('seq-top', '⊤', 'neg');
export const SeqZero = prefix('seq-zero', '0', 'pos');
export const SeqPar = infix('seq-par', '⅋', 'neg');
export const SeqTensor = infix('seq-tensor', '⊗', 'pos');
export const SeqBottom = component('seq-bottom')`
<style type="text/css">
sub {
  vertical-align: sub;
  font-size: 60%;
  line-height: 0;
  color: var(--sequent-neg-colour);
}
</style><seq-symbol neg>⊥<sub class="neg">R</sub><slot></slot></seq-symbol>
`;
export const SeqOne = component('seq-one')`
<style type="text/css">
sub {
  vertical-align: sub;
  font-size: 60%;
  line-height: 0;
  color: var(--sequent-pos-colour);
}
</style><seq-symbol pos>1<sub class="pos">E</sub><slot></slot></seq-symbol>
`;
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
