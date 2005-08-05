\header {


  texidoc = "If @code{strict-note-spacing} is set, then spacing of
notes is not influenced by bars and clefs half-way on the system.
Rather, they are put just before the note that occurs at the same
time.  This may cause collisions.  "

}


\version "2.7.4"

\paper {
  raggedright = ##t
  indent = 0
}
\layout {
  \context {
    \Score
  }
}

\relative c''
<<
  \override Score.SpacingSpanner #'strict-note-spacing = ##t 
  \set Score.proportionalNotationDuration = #(ly:make-moment 1 16)
  \new Staff { c8[ c \clef alto c c c c]  c4 c2 r2 }
  \new Staff { c2  \times 2/3 { c8 \clef bass cis,, c } c4 c1 }
>>
  
