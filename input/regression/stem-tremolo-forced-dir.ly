
\version "2.14.0"

\header {
texidoc = "Tremolo works even when a stem is forced in a
particular direction.
"
}

\layout { ragged-right = ##t }
\relative c 
{
        \clef bass
	\stemUp f4 : 32
	\stemDown g,4 : 32
}
