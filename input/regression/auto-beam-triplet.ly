
\header
{
texidoc = "Automatic beaming is also done on tuplets."
}

\version "2.3.16"

\score{
	\relative c''{
		c8 c c c
		\times 4/6 { c c c c c c}
	}
    \paper { raggedright= ##t }
}
