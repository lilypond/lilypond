\version "2.1.11"
\header {
    title	= "Puer natus est nobis (excerptum)"
    subtitle	= "Antiphona ad introitum VII"
    texidoc = "

Demonstrate gregorian chant notation

This file pretty nicely demonstrates what still does
not work among ligatures: (i) horizontal spacing between ligatures and
lyrics aligment is broken; (ii) lily crashes when removing
Stem_engraver, but still using Slur_engraver (this is useful needed
for the \addlyrics feature when no slurs are to be typeset); (iii)
episem causes a parse error (this used to work a while ago); (iv)
support for augmentum dots is missing; (v) accidentals must be placed
before the ligature (not demonstrated in this example)."

}

#(set-staff-size 26)
\include "gregorian-init.ly"

%%% N.B.: Yes, I know, the formatting of the following looks awful,
%%% but this is intentional for editorial purposes (simplifies some
%%% global search/replace operations in emacs).

cantus = \notes {
  \[ g4
    (		%%% Pu-
    \pes
    d')
  \]
  d'		%%% er
  \[ d'
    (		%%% na-
    \pes e' \flexa
    d')
  \]
  c'		%%% tus
  \[ c'
    (		%%% est
    c'
    c')
  \]
  \[ d'
    (		%%% no-
    \flexa c' e' \flexa
    d')
  \]
  d'            %%% bis,
  \divisioMaior
  \[ g
    (		%%% et
    \pes \deminutum
    d')
  \]
  \[ d'
    (		%%% fi-
    \pes e' \flexa
    d')
  \]
  \[ c'
    (		%%% li-
    \flexa
    b)
  \]
  a		%%% us
  \[ c'
    (		%%% da-
    c' \pes
    d')
  \]
  c'		%%% tus-
  c'		%%% est
  \[ c'
    (		%%% no-
    \pes d' \flexa c'
    c')
  \]
  \[ g
    (		%%% bis:
    \pes a \flexa
    g)
  \]
  \divisioMaxima
}

verba = \context Lyrics = verba \lyrics {
  Pu- er na- tus est no- bis,
  et fi- li- us da- tus est no- bis:
}

\score {
  \context VaticanaVoice <<
    \addlyrics
    \cantus
    \verba
  >>
  \paper {
    stafflinethickness = \staffspace / 7.0
    linewidth = 137.0\mm
    width = 137.0\mm
    indent = 0.0
    raggedright = ##t
    packed = ##t
%   width = 15.0 \cm %%% no effect?

    \translator {
      \ScoreContext
      \remove Bar_number_engraver
%     SpacingSpanner \set #'spacing-increment = #0.5
      timing = ##f

      % Maybe don't do this except for transcription -- it may produce
      % additional space
      barAlways = ##t
%     skipBars = ##t
    }
  }
}
