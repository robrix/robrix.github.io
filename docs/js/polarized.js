import { component, infix } from './sequent.js';

export const SeqWith = infix('seq-with', '&amp;', 'neg');
export const SeqSum = infix('seq-sum', '⊕', 'pos');
export const SeqTop = component('seq-top')`<seq-symbol neg>⊤<slot></slot></seq-symbol>`;
export const SeqZero = component('seq-zero')`<seq-symbol pos>0<slot></slot></seq-symbol>`;
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
</style><seq-op name="⊥" neg><sub slot="op-decoration" class="neg">R</sub><slot></slot></seq-op>
`;
export const SeqOne = component('seq-one')`
<style type="text/css">
sub {
  vertical-align: sub;
  font-size: 60%;
  line-height: 0;
  color: var(--sequent-pos-colour);
}
</style><seq-op name="1" pos><sub slot="op-decoration" class="pos">E</sub><slot></slot></seq-op>
`;
export const SeqImplication = infix('seq-impl', '→', 'neg');
export const SeqCoimplication = infix('seq-coimpl', '⤚', 'pos');
export const SeqNot = component('seq-not')`<seq-op name="¬" prefix neg><slot></slot></seq-op>`;
export const SeqNegate = component('seq-negate')`<seq-op name="∼" prefix pos><slot></slot></seq-op>`;
export const SeqNotUntrue = component('seq-not-untrue')`<seq-op name="¬̷" prefix neg><slot></slot></seq-op>`;
export const SeqTrue = component('seq-true')`<seq-op name="✓" prefix pos><slot></slot></seq-op>`;
export const SeqUpShift = component('seq-up')`<seq-op name="↑" prefix neg><slot></slot></seq-op>`;
export const SeqDownShift = component('seq-down')`<seq-op name="↓" prefix pos><slot></slot></seq-op>`;
export const SeqForAll = component('seq-forall')`<seq-op name="∀" prefix neg><slot></slot></seq-op>`;
export const SeqExists = component('seq-exists')`<seq-op name="∃" prefix pos><slot></slot></seq-op>`;
