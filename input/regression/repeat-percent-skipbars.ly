\version "2.1.28"


\header {
texidoc = "Percent repeats are not skipped, even when @code{skipBars} is set."
}

    \paper { raggedright= ##t }

\score {
     \context Staff <<
	\set Score.skipBars = ##t
	\notes {
	\repeat "percent" 2 { g2 a g a }
	}
     >>
}

