\version "1.5.68"

\header {
  texidoc ="Polyphonic piano music"
}

upperOne = \notes\relative c'' {
  \voiceOne
  a b c d
}

upperTwo = \notes\relative c' {
  \voiceTwo
  a2 c
}

lowerOne = \notes\relative c {
  \voiceOne
  a2 c
}

lowerTwo = \notes\relative c {
  \voiceTwo
  a1
}

\score {
  \context PianoStaff <
    %\time 4/4
    \context Staff = upper <
      \context Voice = one \upperOne
      \context Voice = two \upperTwo
    >  
    \context Staff = lower <
      \clef bass
      \context Voice = one \lowerOne
      \context Voice = two \lowerTwo
    >  
  >
  \paper { }  
  \midi { }  
}
