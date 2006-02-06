\version "2.7.32"
\header {
    texidoc  = "@cindex Gregorian Scripts

Here is demonstrated a preliminary support of Gregorian Scripts:

ictus, circulus, semicirculus, accentus, episem.

"}

%{

TODO: augmentum.  Syntax: either as bracket (\augmentumInitium,
\augmentumFinis), or as head prefix with subsequently collecting all
dots and putting them behind the ligature in a vertical row.
Counterexample to the second approach: Graduale Triplex, tempus per
annum, hebdomada septima, alleluia (page 280).

FIXME: horizontal spacing (ragged right mode).

FIXME: padding/minimum-distance is fragile.

FIXME: episem stops one notehead too early.

%}

\include "gregorian-init.ly"

\paper {
    line-thickness = \staff-space / 5.0
}

\score {
  \context VaticanaVoice {
    \override Script #'padding = #-0.5
     {
      a\ictus
      a\circulus
      a\semicirculus
      a\accentus

      %{ %% TODO: augmentum:
	a\augmentum
	\[ \augmentumInitium b \flexa a \augmentumFinis \]
      %}

      \[ a \episemInitium \pes b \flexa a \episemFinis \]
    }
  }
  \layout {
    line-width = 70.0
    width = 60.0
    indent = 0.0
    ragged-right = ##t
  }
}
