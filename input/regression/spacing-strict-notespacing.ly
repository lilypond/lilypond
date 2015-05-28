\header {


  texidoc = "If @code{strict-note-spacing} is set, then spacing of
notes is not influenced by bars and clefs half-way on the system.
Rather, they are put just before the note that occurs at the same
time.  This may cause collisions.  "

}


\version "2.19.21"

\paper {
  ragged-right = ##t
  indent = 0
}
\layout {
  \context {
    \Score
  }
}

\relative
<<
  \override Score.SpacingSpanner.strict-note-spacing = ##t 
  \set Score.proportionalNotationDuration = #(ly:make-moment 1/16)
  \new Staff {
    c''8[ c \clef alto c c \grace { d16 }  c8 c]  c4 c2
    \grace { c16 c16 }
    c2 }
  \new Staff {
    c2  \tuplet 3/2 { c8 \clef bass cis,, c } 
    c4
    c1
  }
>>
  
