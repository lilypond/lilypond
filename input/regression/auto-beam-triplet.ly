
\header
{
texidoc = "Automatic beaming is also done on tuplets."
}

\version "2.2.0"

\score{
	\notes\relative c''{
		c8 c c c
		\times 4/6 { c c c c c c}
	}
    \paper { raggedright= ##t }
}
