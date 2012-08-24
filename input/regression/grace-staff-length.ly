\version "2.16.0"
\header{
  texidoc = "Stripped version of trip.ly.  Staves should be of correct length."
}

\layout { ragged-right= ##t }


\context PianoStaff  \relative c'' <<
  \new Staff {
    r1
    r1
    \bar "|."
  }
  \new Staff {
    r1
    \context Staff {
      \grace { c16 } c1
    }
  }
>> 
\layout { }


