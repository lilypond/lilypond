%% DO NOT EDIT this file manually; it is automatically
%% generated from LSR http://lsr.dsi.unimi.it
%% Make any changes in LSR itself, or in Documentation/snippets/new/ ,
%% and then run scripts/auxiliar/makelsr.py
%%
%% This file is in the public domain.
\version "2.17.11"

\header {
  lsrtags = "chords, editorial-annotations, rhythms, tweaks-and-overrides"

  texidoc = "
Fingerings and string numbers applied to individual notes will
automatically avoid beams and stems, but this is not true by default
for fingerings and string numbers applied to the individual notes of
chords.  The following example shows how this default behavior can be
overridden.

"
  doctitle = "Avoiding collisions with chord fingerings"
} % begin verbatim


\relative c' {
  \set fingeringOrientations = #'(up)
  \set stringNumberOrientations = #'(up)
  \set strokeFingerOrientations = #'(up)

  % Default behavior
  r8
  <f c'-5>8
  <f c'\5>8
  <f c'-\rightHandFinger #2 >8

  % Corrected to avoid collisions
  r8
  \override Fingering.add-stem-support = ##t
  <f c'-5>8
  \override StringNumber.add-stem-support = ##t
  <f c'\5>8
  \override StrokeFinger.add-stem-support = ##t
  <f c'-\rightHandFinger #2 >8
}
