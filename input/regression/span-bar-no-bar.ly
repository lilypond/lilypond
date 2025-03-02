\version "2.25.25"

\header {
  texidoc = "Span bars avoid staves that have no bar lines.

At the end of measure@tie{}1, only the top two staves should be spanned.
At the end of measure@tie{}2, only the top three staves should be spanned.
At the end of measure@tie{}3, all staves should be spanned."
}

#(ly:set-option 'warning-as-error #t)

\layout {
  \context {
    \Staff
    instrumentName = "Staff"
  }
  \context {
    \GregorianTranscriptionStaff
    instrumentName = \markup \left-column { "Gregor." "Transcr." }
  }
  \context {
    \PetrucciStaff
    instrumentName = "Petrucci"
  }
}

music = \fixed c' { b1 b1 \caesura b1 \section }

\new StaffGroup <<
  \new Staff \music
  \new Lyrics \lyrics { It1 is1 my1 }
  \new Dynamics { s1\< s1 s1 <>\! }
  \new Staff \music
  \new PetrucciStaff \music
  \new GregorianTranscriptionStaff \music
  \new Staff \music
>>
