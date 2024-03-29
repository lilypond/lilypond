%% DO NOT EDIT this file manually; it was automatically
%% generated from the LilyPond Snippet Repository
%% (http://lsr.di.unimi.it).
%%
%% Make any changes in the LSR itself, or in
%% `Documentation/snippets/new/`, then run
%% `scripts/auxiliar/makelsr.pl`.
%%
%% This file is in the public domain.

\version "2.24.0"

\header {
  lsrtags = "ancient-notation, contexts-and-engravers, staff-notation, tweaks-and-overrides"

  texidoc = "
@emph{Mensurstriche}, bar lines between but not through staves, can be
printed by setting @code{measureBarType} to @code{\"-span|\"} and using a
grouping context that allows span bars, such as @code{StaffGroup}.
"

  doctitle = "Mensurstriche layout (bar lines between the staves)"
} % begin verbatim


\layout {
  \context {
    \Staff
    measureBarType = "-span|"
  }
}

music = \fixed c'' {
  c1
  d2 \section e2
  f1 \fine
}

\new StaffGroup <<
  \new Staff \music
  \new Staff \music
>>
