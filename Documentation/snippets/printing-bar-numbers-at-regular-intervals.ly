%% DO NOT EDIT this file manually; it was automatically
%% generated from `Documentation/snippets/new/`.
%%
%% Make any changes in `Documentation/snippets/new/`,
%% then run `scripts/auxiliar/makelsr.pl --new`.
%%
%% This file is in the public domain.
%%
%% Note: this file works from version 2.23.7.

\version "2.23.12"

\header {
  lsrtags = "rhythms"

  texidoc = "
By setting the @code{barNumberVisibility} property, bar numbers can be
printed at regular intervals. Here the bar numbers are printed every
two measures except at the end of the line.
"

  doctitle = "Printing bar numbers at regular intervals"
} % begin verbatim


\relative c' {
  \override Score.BarNumber.break-visibility = #end-of-line-invisible
  \set Score.currentBarNumber = #11
  % Print a bar number every second measure
  \set Score.barNumberVisibility = #(every-nth-bar-number-visible 2)
  c1 | c | c | c | c
  \break
  c1 | c | c | c | c
}
