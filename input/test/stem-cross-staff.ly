\version "2.3.4"

\header { texidoc = "@cindex Stem Cross Staff
The chords which exceptionally cross staves may be produced by increasing 
the length of the stem in the lower stave, so it reaches the stem in the 
upper stave, or vice versa. "
}

stemExtend = \once \override Stem  #'length = #22

%% following reqs 1.7.1 or better.
noFlag = \once \override Stem  #'flag-style = #'no-flag


\score {  
    \context  PianoStaff
        << \new Staff   {
	    \stemDown
	    \stemExtend
	    f'4
	    \stemExtend
	    \noFlag
	    f'8 }
          \new Staff {
	      \clef bass
	      a4 a8 }
	  >>

    \paper { raggedright = ##t}
}

