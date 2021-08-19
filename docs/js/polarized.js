import { component } from './sequent.js';

export const SeqWith = component('seq-with')`<seq-op name="&amp;" neg><slot></slot></seq-op>`;
export const SeqSum = component('seq-sum')`<seq-op name="⊕" pos><slot></slot></seq-op>`;
export const SeqTop = component('seq-top')`<seq-op name="⊤" neg><slot></slot></seq-op>`;
export const SeqZero = component('seq-zero')`<seq-op name="0" pos><slot></slot></seq-op>`;
export const SeqPar = component('seq-par')`<seq-op name="⅋" neg><slot></slot></seq-op>`;
export const SeqTensor = component('seq-tensor')`<seq-op name="⊗" pos><slot></slot></seq-op>`;
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
export const SeqImplication = component('seq-impl')`<seq-op name="→" neg><slot></slot></seq-op>`;
export const SeqCoimplication = component('seq-coimpl')`<seq-op name="⤚" pos><slot></slot></seq-op>`;
export const SeqNot = component('seq-not')`<seq-op name="¬" neg><slot></slot></seq-op>`;
export const SeqNegate = component('seq-negate')`<seq-op name="∼" pos><slot></slot></seq-op>`;
export const SeqNotUntrue = component('seq-not-untrue')`<seq-op name="¬̷" neg><slot></slot></seq-op>`;
export const SeqTrue = component('seq-true')`<seq-op name="✓" pos><slot></slot></seq-op>`;
export const SeqUpShift = component('seq-up')`<seq-op name="↑" neg><slot></slot></seq-op>`;
export const SeqDownShift = component('seq-down')`<seq-op name="↓" pos><slot></slot></seq-op>`;
export const SeqForAll = component('seq-forall')`<seq-op name="∀" neg><slot></slot></seq-op>`;
export const SeqExists = component('seq-exists')`<seq-op name="∃" pos><slot></slot></seq-op>`;
