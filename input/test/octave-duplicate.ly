\header {
texidoc="Octave duplicate parts of music"
}

upperOne = \notes\relative c'' {
  a4 a a a
  a4 a a a
  a4 a a a
  a4 a a a
}

upperOctave = \notes <
  \context Thread=upperOne { \transpose c \upperOne }
  \context Thread=upperOne {
    \property Thread.devNullThread = #'allways s1*2
    \property Thread.devNullThread = #'() s1*2
  }
>

lowerOne = \notes\relative c {
  a8 a a a  a a a a
  a8 a a a  a a a a
  a8 a a a  a a a a
  a8 a a a  a a a a
}
  
firstEight = \notes { 
  \property Thread.devNullThread = #'() s8
  \property Thread.devNullThread = #'allways s8*7
}

lowerOctave = \notes <
  \context Thread=lowerOne { \transpose c  \lowerOne }
  \context Thread=lowerOne {
    \repeat "unfold" 4 { \firstEight }
  }
>

\score {
  <
    \context PianoStaff <
      \context Staff=upper <
        \context Voice=upperOne <
          \upperOne
          \upperOctave
        >
      >
      \context Staff=lower <
        \clef bass
	\context Voice=lowerOne <
	  \lowerOne
	  \lowerOctave
	>
      >
    >
  >  
  \paper { }
}
