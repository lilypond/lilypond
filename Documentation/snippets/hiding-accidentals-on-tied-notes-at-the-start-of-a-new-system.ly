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
  lsrtags = "pitches"

  texidoc = "
This shows how to hide accidentals on tied notes at the start of a new
system.
"

  doctitle = "Hiding accidentals on tied notes at the start of a new system"
} % begin verbatim


\relative c'' {
  \override Accidental.hide-tied-accidental-after-break = ##t
  cis1~ cis~
  \break
  cis
}
