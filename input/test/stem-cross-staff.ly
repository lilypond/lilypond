\header {
    texidoc = "Cross staff stems

Unfortunately, there is no support for putting chords across staves.
You can get this result by increasing the length of the stem in the
lower stave so it reaches the stem in the upper stave, or vice versa.

@cindex  Cross staff stems
@cindex Stems, cross staff

"
}

stemExtend = \once \property Voice.Stem \override #'length  = #22

%% following reqs 1.7.1 or better.
noFlag = \once \property Voice.Stem \override #'flag-style = #'no-flag


\score { \notes 
    \context  PianoStaff
        < \context Staff = up   {
	    \stemDown
	    \stemExtend
	    f'4
	    \stemExtend
	    \noFlag
	    f'8 }
          \context Staff = down {
	      \clef bass
	      a4 a8 }
	  >

    \paper { linewidth = -1.0 }
}
