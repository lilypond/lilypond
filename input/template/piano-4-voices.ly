\version "2.3.22"

\header {
  texidoc ="Polyphonic piano music."
}

upperOne = \relative c'' {
  \voiceOne
  a b c d
}

upperTwo = \relative c' {
  \voiceTwo
  a2 c
}

lowerOne = \relative c {
  \voiceOne
  a2 c
}

lowerTwo = \relative c {
  \voiceTwo
  a1
}

\score {
  \context PianoStaff <<
    %\time 4/4
    \context Staff = upper <<
      \context Voice = one \upperOne
      \context Voice = two \upperTwo
    >>  
    \context Staff = lower <<
      \clef bass
      \context Voice = one \lowerOne
      \context Voice = two \lowerTwo
    >>  
  >>
  \layout { }  
  \midi { }  
}
