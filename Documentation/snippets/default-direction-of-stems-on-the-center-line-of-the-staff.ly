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
  lsrtags = "editorial-annotations"

  texidoc = "
The default direction of stems on the center line of the staff is set
by the @code{Stem} property @code{neutral-direction}.
"

  doctitle = "Default direction of stems on the center line of the staff"
} % begin verbatim


\relative c'' {
  a4 b c b
  \override Stem.neutral-direction = #up
  a4 b c b
  \override Stem.neutral-direction = #down
  a4 b c b
}
