\version "2.3.16"


\header {
texidoc = "Percent repeats are not skipped, even when @code{skipBars} is set."
}

    \paper { raggedright= ##t }

\score {
     \context Staff <<
	\set Score.skipBars = ##t
	 {
	\repeat "percent" 2 { g2 a g a }
	}
     >>
}

