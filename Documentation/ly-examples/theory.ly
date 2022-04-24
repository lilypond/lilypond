\version "2.19.21"
\include "example-header.ily"

#(ly:set-option 'point-and-click #f)
#(set-global-staff-size 24)

global = {
    \time 4/4
    \numericTimeSignature
    \key c \major
}

cf = \relative {
  \clef bass
  \global
  c4 c' b a |
  g a f d |
  e f g g, |
  c1
}

upper = \relative c'' {
  \global
  r4 s4 s2 |
  s1*2 |
  s2 s4 s
  \bar "||"
}

bassFigures = \figuremode {
  s1*2 | s4 <6> <6 4> <7> | s1
}

\markup { "Exercise 3: Write 8th notes against the given bass line." }

\score {
  \new PianoStaff <<
    \new Staff {
      \context Voice = "added voice" \with {
        \consists Balloon_engraver
      }
      \upper
    }

    \new Staff = lower {
      <<
%      \context Voice = "cantus firmus" \with {
%        \consists Balloon_engraver
%      }
        \context Staff = lower \cf
        \new FiguredBass \bassFigures
      >>
    }
  >>
  \layout {}
  %{\midi {
    \tempo 4 = 120
  }%}
}
