\version "2.1.7"


\header {
texidoc = "Percent repeats are not skipped, even when skipBars is set.  "
}

    \paper { raggedright= ##t }

\score {
     \context Staff <<
	\property Score.skipBars = ##t
	\notes {
	\repeat "percent" 2 { g2 a g a }
	}
     >>
}

