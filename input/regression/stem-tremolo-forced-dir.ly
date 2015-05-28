
\version "2.19.21"

\header {
texidoc = "Tremolo works even when a stem is forced in a
particular direction.
"
}

\layout { ragged-right = ##t }
\relative 
{
        \clef bass
	\stemUp f4 : 32
	\stemDown g,4 : 32
}
