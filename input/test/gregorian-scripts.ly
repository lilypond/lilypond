\version "1.9.8"
\header {
    title	= "Gregorian Scripts"
    texidoc     = "@cindex Gregorian Scripts

Gregorian Scripts:

ictus, circulus, semicirculus, accentus, episem.

TODO: augmentum.  Syntax: either as bracket (\augmentumInitium,
\augmentumFinis), or as head prefix with subsequently collecting all
dots and putting them behind the ligature in a vertical row.
Counterexample to the second approach: Graduale Triplex, tempus per
annum, hebdomada septima, alleluia (page 280).

FIXME: horizontal spacing (ragged right mode).

FIXME: padding/minimum-distance is fragile.

"}

\include "gregorian-init.ly"

\score {
  \context VaticanaVoice {
    \property VaticanaVoice.Script \set #'padding = #-0.5
    \notes {
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
  \paper {
    linewidth = 70.0
    stafflinethickness = \staffspace / 5.0
    width = 60.0
    indent = 0.0
    raggedright = ##t
  }
}
