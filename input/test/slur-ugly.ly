
\version "2.3.2"

\header { texidoc="@cindex Slur Ugly
Strange slurs can be produced by setting properties by hand. "
}

baseWalk = \notes \relative c {
  d,8( a' d f a d f d a f d  a)
}

\score {
  \notes \context PianoStaff <<
    \time 6/4
    \context Staff=up { s1 * 6/4 }
    \context Staff=down <<
      \clef bass
      #(override-auto-beam-setting '(end * * * *)  1 2 'Score)
      \autochange  \context Voice \baseWalk
    >>
  >>
  \paper {
    raggedright = ##t
    \context {
      \Voice
      \override Slur #'beautiful = #5.0
      \override Slur #'direction = #1
      \override Stem #'direction = #-1
    }
    \context {
      \PianoStaff
      \override VerticalAlignment #'threshold = #'(5 . 5)
    }
  }
}


