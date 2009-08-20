\version "2.12.0"
\include "example-header.ily"

#(ly:set-option 'point-and-click #f)

global = {
    \time 4/4 
    \numericTimeSignature
    \key c \major
    #(set-global-staff-size 24)
}

cf = \relative c { 
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
        \consists "Balloon_engraver"
      }
      \upper
    }

    \new Staff = lower { 
      <<
%      \context Voice = "cantus firmus" \with {
%        \consists "Balloon_engraver"
%      }
        \context Staff = lower \cf
        \new FiguredBass \bassFigures
      >>
    }
  >>
  \layout {}
  %{\midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 120 4)
    }  
  }%}
}

