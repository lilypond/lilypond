%%% G.F Haendel, Giulio Cesare in Egitto
%%% Act I, scene IV
%%% Sesto: Svegliatevi nel core, furie d'un alma offesa (excerpt)
%%%
%%% Nicolas Sceaux <nicolas.sceaux@free.fr>

\version "2.21.0"
\include "sesto.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Reduction
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\book {
  \header { instrument = "Vocal part and keyboard reduction" }
  \paper {
    #(layout-set-staff-size 16)
  }
  \score {
    <<
      \new Staff <<
        \new Voice = "sesto" \with { autoBeaming = ##f } <<
          \set Staff.instrumentName = \markup \smallCaps Sesto.
          \global \clef treble \sesto
        >>
        \new Lyrics \lyricsto "sesto" \sestoLyrics
      >>
      \new PianoStaff <<
        \new Staff <<
          \global \clef treble
          \partCombine \keepWithTag #'reduction \violinoI \keepWithTag #'reduction \violinoII
        >>
        \new Staff <<
          \global \clef bass \bassi
        >>
      >>
    >>
    \layout {
      indent = 20\mm
      \context { \Voice printPartCombineTexts = ##f }
    }
  }
}
