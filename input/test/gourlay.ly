#(ly:set-option 'old-relative)
\version "1.9.2"
\header{

    texidoc="@cindex Gourlay

This is taken from Gourlay's paper on
breaking lines.  "

}

\score{
       \notes \context Staff  \relative c'' <
	    { \stemUp d2 d     | d d | d4 d2. | \break  c1 }
	    \\
	    { \stemDown g4 g g g | \times 2/3 { g2 g2 g2 } | g4. g8 g2 | c1 }
	>
	\paper{
		raggedright = ##t
	}
}


