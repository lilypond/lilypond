\version "1.5.68"

\include "os-music.ly"
\include "paper16.ly"

\score {
  \context Staff <
    \property Score.skipBars = ##t
    \property Staff.midiInstrument = #"flute"
    \global
    \Key
    \flautoII
  >
  \header {
    instrument = "flute I"
  }
  \paper {
    linewidth = 80 * \staffspace
    textheight = 40 * \staffspace
  }
  \midi {
    \tempo 4 = 75
  }
}


% switch off settings of -score file, so the rest of the
% tutorial isn't \special
#(set! point-and-click #f)
