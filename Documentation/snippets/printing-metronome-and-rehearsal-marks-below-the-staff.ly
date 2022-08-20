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
  lsrtags = "expressive-marks, rhythms, tweaks-and-overrides"

  texidoc = "
By default, metronome and rehearsal marks are printed above the
staff.  To place them below the staff simply set the @code{direction}
property of @code{MetronomeMark} or @code{RehearsalMark} appropriately.
"

  doctitle = "Printing metronome and rehearsal marks below the staff"
} % begin verbatim


\layout {
  indent = 0
  ragged-right = ##f
}

{
  % Metronome marks below the staff
  \override Score.MetronomeMark.direction = #DOWN
  \tempo 8. = 120
  c''1

  % Rehearsal marks below the staff
  \override Score.RehearsalMark.direction = #DOWN
  \mark \default
  c''1
}
