\version "2.3.8"
\header{

    texidoc="@cindex Gourlay

The breaking of line works also with polyphony. This is taken from 
Gourlay's paper on breaking lines.  "

}

\score{
        \context Staff  \relative c'' <<
	    { \stemUp d2 d     | d d | d4 d2. | \break  c1 }
	    \\
	    { \stemDown g4 g g g | \times 2/3 { g2 g2 g2 } | g4. g8 g2 | c1 }
	>>
	\paper{
		raggedright = ##t
	}
}


