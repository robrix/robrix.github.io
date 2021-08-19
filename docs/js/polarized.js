import { component } from './sequent.js';

export const SeqNot = component('seq-not')`<seq-op name="¬" neg><slot></slot></seq-op>`;
export const SeqNegate = component('seq-negate')`<seq-op name="∼" pos><slot></slot></seq-op>`;
