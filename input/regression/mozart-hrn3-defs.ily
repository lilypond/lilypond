% 

longgrace = \override Flag.stroke-style = #'()
endlonggrace = \revert Flag.stroke-style
ritenuto = \markup { \italic  "rit." }

\version "2.21.0"

\layout {
  \context {
    \Score
    skipBars = ##t
    midiInstrument = "french horn"
    %% try to mimic Breitkopf
    \override RehearsalMark.padding = #1
    restNumberThreshold = #1

    \override RehearsalMark.font-size = #4.5
  }
}

\paper{
  indent = 10\mm
  line-width = 189\mm
  ragged-last-bottom = ##f
}

