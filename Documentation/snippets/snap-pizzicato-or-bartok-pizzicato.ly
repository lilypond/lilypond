%% DO NOT EDIT this file manually; it was automatically
%% generated from the LilyPond Snippet Repository
%% (http://lsr.di.unimi.it).
%%
%% Make any changes in the LSR itself, or in
%% `Documentation/snippets/new/`, then run
%% `scripts/auxiliar/makelsr.pl`.
%%
%% This file is in the public domain.

\version "2.23.12"

\header {
  lsrtags = "expressive-marks, unfretted-strings"

  texidoc = "
A snap-pizzicato (also known as @qq{Bartok pizzicato}) is a @qq{strong
pizzicato where the string is plucked vertically by snapping and
rebounds off the fingerboard of the instrument} (Wikipedia).  It is
denoted by a circle with a vertical line going from the center upwards
outside the circle.
"

  doctitle = "Snap-pizzicato or Bartok pizzicato"
} % begin verbatim


\relative c' {
  c4\snappizzicato
  <c' e g>4\snappizzicato
  <c' e g>4^\snappizzicato
  <c, e g>4_\snappizzicato
}
