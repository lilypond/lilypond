\version "2.10.0"
\header {
    texidoc  = "@cindex Gregorian Scripts

Here is demonstrated a preliminary support of Gregorian Scripts:

ictus, circulus, semicirculus, accentus, episem.

"}

%{

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
