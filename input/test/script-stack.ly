
\version "2.3.8"

\header { texidoc = "@cindex Stacked Text Articulation
Text and articulations may be stacked on top of each other. "
}

\score {
	\relative c' {
		c2_"geen gedonder"_"jij gaat onder"
		c2^^^.^|^"down"^"up"
	 }
	\paper { raggedright = ##t}
}

