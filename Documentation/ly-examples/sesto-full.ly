%%% G.F Haendel, Giulio Cesare in Egitto
%%% Act I, scene IV
%%% Sesto: Svegliatevi nel core, furie d'un alma offesa (excerpt)
%%%
%%% Nicolas Sceaux <nicolas.sceaux@free.fr>

\version "2.19.2"
\include "sesto.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Lead sheet
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\book {
  \paper {
    #(layout-set-staff-size 14)
  }
  \score {
    \new StaffGroupNoBar <<
      \new StaffGroupNoBracket <<
        \new Staff <<
          \set Staff.instrumentName = "Violino I."
          \global \clef treble \keepWithTag #'violin \violinoI
        >>
        \new Staff <<
          \set Staff.instrumentName = "Violino II."
          \global \clef treble \keepWithTag #'violin \violinoII
        >>
      >>
      \new Staff <<
        \new Voice = "sesto" \with { autoBeaming = ##f } <<
          \set Staff.instrumentName = \markup \smallCaps Sesto.
          \global \clef treble \sesto
        >>
        \new Lyrics \lyricsto "sesto" \sestoLyrics
      >>
      \new Staff <<
        \set Staff.instrumentName = "Bassi."
        \global \clef bass \bassi
      >>
    >>
    \layout {
      indent = 20\mm
      \context {
        \Score
        \accepts StaffGroupNoBar
        skipBars = ##t
      }
      \context {
        \StaffGroup
        \name StaffGroupNoBar
        \description "Like StaffGroup, but without spanbar"
        \remove Span_bar_engraver
        \accepts StaffGroupNoBracket
      }
      \context {
        \StaffGroup
        \name StaffGroupNoBracket
        \description "Like StaffGroup, but without brackets"
        \remove System_start_delimiter_engraver
      }
    }
%    \midi { }
  }
}
