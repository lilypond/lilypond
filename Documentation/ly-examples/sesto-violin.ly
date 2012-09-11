%%% G.F Haendel, Giulio Cesare in Egitto
%%% Act I, scene IV
%%% Sesto: Svegliatevi nel core, furie d'un alma offesa (excerpt)
%%%
%%% Nicolas Sceaux <nicolas.sceaux@free.fr>

\version "2.16.0"
\include "sesto.ily"

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Violino I
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\book {
  \header { instrument = "Violino I" }
  \paper {
    #(layout-set-staff-size 18)
  }
  \score {
    \new Staff <<
      \global \clef treble \keepWithTag #'violin \violinoI
    >>
    \layout { indent = 5\mm }
  }
}
