% tests drum notation and midi-drums.
% see ly/drumpitch.ly for list of instruments and paper-kits.
%     scm/midi.scm for list of midi-drumkits.

\include "drumpitch.ly"

\version "1.3.146"

drh = \notes { cymc4.^"crash" hhc16^"h.h." hh \repeat "unfold" 5 {hhc8 hho hhc8 hh16 hh} hhc4 r4 r2 }
drl = \notes {\repeat "unfold" 3 {bd4 sn8 bd bd4 <bd ss>} bd8 tommh tommh bd toml toml bd tomfh16 tomfh }
timb = \notes \repeat "unfold" 2 {timh4 ssh timl8 ssh r timh r4 ssh8 timl r4 cb8 cb}

\score { \repeat "volta" 2
 <
  \context TwoLineStaff=timbst \notes <
    \property Staff.instrument="timbales"
    \clef "percussion"
    \apply #(drums->paper 'timbales) \timb
  >
  \context Staff=drumst \notes <
    \property Staff.instrument="drums"
    \clef "percussion"
    \apply #(drums->paper 'drums) <
      \context Voice=voa {\stemUp \drh }
      \context Voice=vob {\stemDown \drl }
    >
  >
 >
 \paper {
   \translator {
      \StaffContext
      \consists Instrument_name_engraver
      Script \override #'padding = #0.5
   }
   \translator {
      \StaffContext
      \name TwoLineStaff
      \alias Staff
      \consists Instrument_name_engraver
      StaffSymbol \override #'line-count = #2
      BarLine \override #'bar-size = #2
   }
   \translator {
      \ScoreContext
      \accepts TwoLineStaff
   }
 }
}

\score { \repeat "unfold" 2
  \context Staff \notes <
    \property Staff.instrument="drums"
    \timb
    \drh
    \drl
  >
  \midi{ \tempo 4=120 }
}

