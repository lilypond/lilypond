\version "1.3.146"
\header{
texidoc="
A sharp sign after a double sharp sign, as well as a flat sign
after a double flat sign is automatically prepended with a
natural sign.
"
}



thenotes =  \notes \relative cis' { \time 4/4
gisis'4 gis gisis ges |
geses ges geses gis |
gisis g geses g |
gis ges gis g |
\key a \major
gisis4 gis gisis ges |
geses ges geses gis |
gisis g geses g |
gis ges gis g |
}

\score { < \context Staff \thenotes
	\context NoteNames  {
		\property NoteNames.NoteName \override  #'no-spacing-rods  = ##f 
		\thenotes
	}
	>
}
