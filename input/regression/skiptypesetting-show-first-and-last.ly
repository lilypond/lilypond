\header {

  texidoc = "@code{showFirstLength} and @code{showLastLength} may be set at the
  same time; both the beginning and the end of the score will be printed."

  }

\version "2.23.8"

showFirstLength = R1*1
showLastLength = R1*2
\paper {
  ragged-right = ##T
}

\layout {
  \context {
    \Score
    \remove "Timing_translator"
    \remove "Repeat_acknowledge_engraver"
  }
  \context {
    \Staff
    \consists "Timing_translator"
    \consists "Repeat_acknowledge_engraver"
  }
}

\new PianoStaff <<
  \new Staff \fixed c' {
    \time 1/1
    c1
    %% cut starts here
    \segnoMark 1 \repeat volta 2 {
      d1 e f \breathe
      %% cut ends here
      g1 a
    }
  }
  \new Staff \fixed c {
    \clef "bass"
    \time 3/4
    c4 4 4
    d4 \breathe
    %% cut starts here
    d4 4
    e4 4 4
    f4 4 4
    g4 4 4
    a4
    %% cut ends here
    \clef "tenor" \codaMark 1 \repeat volta 2 {
      a4 4
      b4 4 4
      c'4 4 4
    }
  }
>>
