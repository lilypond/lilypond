\version "2.3.22"


\header {
texidoc = "Percent repeats are not skipped, even when @code{skipBars} is set."
}

    \layout { raggedright= ##t }

\score {
     \context Staff <<
	\set Score.skipBars = ##t
	 {
	\repeat "percent" 2 { g2 a g a }
	}
     >>
}

