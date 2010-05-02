% Do not edit this file; it is automatically
% generated from Documentation/snippets/new
% This file is in the public domain.
%% Note: this file works from version 2.13.4
\version "2.13.18"

\header {
%% Translation of GIT committish: ee2fdacf2ff3acd7e6fb7c4005dfe698b1cc4eed

  texidoc = "
Se puede simular un @qq{hammer} o ligado ascendente con ligaduras de
expresión.

"

  doctitle = "Simular un hammer o ligado ascendente en una tablatura"

  lsrtags = "fretted-strings"
  texidoc = "
A hammer in tablature can be faked with slurs.
"
  doctitle = "Faking a hammer in tablatures"
} % begin verbatim


\score {
  \new TabStaff {
    \relative c'' {
      \tabFullNotation
      c4( d) d( d)
      d2( c)
    }
  }
}
