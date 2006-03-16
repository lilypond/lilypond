\version "2.7.39"
\header {
    title	= "Puer natus est nobis (excerptum)"
    subtitle	= "Antiphona ad introitum VII"
    texidoc = "

Demonstrate gregorian chant notation

This file pretty nicely demonstrates what still does
not work among ligatures: (i) horizontal spacing between ligatures and
lyrics aligment is broken; (ii) lily crashes when removing
Stem_engraver, but still using Slur_engraver (this is useful needed
for the \oldaddlyrics feature when no slurs are to be typeset); (iii)
episem causes a parse error (this used to work a while ago); (iv)
support for augmentum dots is missing; (v) accidentals must be placed
before the ligature (not demonstrated in this example)."

}

#(set-global-staff-size 26)
\include "gregorian-init.ly"

%%% N.B.: Yes, I know, the formatting of the following looks awful,
%%% but this is intentional for editorial purposes (simplifies some
%%% global search/replace operations in emacs).

cantus = \new VaticanaVoice = "cantus"  {
  \[ g4\melisma %%% Pu-
    \pes
    d'\melismaEnd
  \]
  d'		%%% er
  \[ d'\melisma %%% na-
    \pes e' \flexa
    d'\melismaEnd
  \]
  c'		%%% tus
  \[ c'\melisma %%% est
    c'
    c'\melismaEnd
  \]
  \[ d'\melisma %%% no-
    \flexa c' e' \flexa
    d'\melismaEnd
  \]
  d'            %%% bis,
  \divisioMaior
  \[ g\melisma %%% et
    \pes \deminutum
    d'\melismaEnd
  \]
  \[ d'\melisma %%% fi-
    \pes e' \flexa
    d'\melismaEnd
  \]
  \[ c'\melisma %%% li-
    \flexa
    b\melismaEnd
  \]
  a		%%% us
  \[ c'\melisma %%% da-
    c' \pes
    d'\melismaEnd
  \]
  c'		%%% tus-
  c'		%%% est
  \[ c'\melisma %%% no-
    \pes d' \flexa c'
    c'\melismaEnd
  \]
  \[ g\melisma %%% bis:
    \pes a \flexa
    g\melismaEnd
  \]
  \divisioMaxima
}

verba = \new Lyrics = "verba" \lyricmode {
  Pu -- er na -- tus est no -- bis,
  et fi -- li -- us da -- tus est no -- bis:
}

\paper  {
    line-thickness = \staff-space / 7.0
}

\score {
  <<
    \cantus
    \lyricsto "cantus" \verba
  >>
  \layout {
    line-width = 137.0\mm
    width = 137.0\mm
    indent = 0.0
    ragged-right = ##t
    packed = ##t
    \context {
      \Score
      \remove Bar_number_engraver
%     \override SpacingSpanner #'spacing-increment = #0.5
      timing = ##f

      % Maybe don't do this except for transcription -- it may produce
      % additional space
      barAlways = ##t
%     skipBars = ##t
    }
  }
}
