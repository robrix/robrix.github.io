import { component } from './sequent.js';

export const SeqWith = component('seq-with')`<seq-op name="&amp;" neg><slot></slot></seq-op>`;
export const SeqSum = component('seq-sum')`<seq-op name="⊕" pos><slot></slot></seq-op>`;
export const SeqTop = component('seq-top')`<seq-op name="⊤" neg><slot></slot></seq-op>`;
export const SeqZero = component('seq-zero')`<seq-op name="0" pos><slot></slot></seq-op>`;
export const SeqPar = component('seq-par')`<seq-op name="⅋" neg><slot></slot></seq-op>`;
export const SeqTensor = component('seq-tensor')`<seq-op name="⊗" pos><slot></slot></seq-op>`;
export const SeqBottom = component('seq-bottom')`<seq-op name="⊥" neg><slot></slot></seq-op>`;
export const SeqOne = component('seq-one')`<seq-op name="1" pos><slot></slot></seq-op>`;
export const SeqImplication = component('seq-impl')`<seq-op name="→" neg><slot></slot></seq-op>`;
export const SeqCoimplication = component('seq-coimpl')`<seq-op name="⤚" pos><slot></slot></seq-op>`;
export const SeqNot = component('seq-not')`<seq-op name="¬" neg><slot></slot></seq-op>`;
export const SeqNegate = component('seq-negate')`<seq-op name="∼" pos><slot></slot></seq-op>`;
export const SeqNotUntrue = component('seq-not-untrue')`<seq-op name="¬̷" neg><slot></slot></seq-op>`;
export const SeqTrue = component('seq-true')`<seq-op name="✓" pos><slot></slot></seq-op>`;
