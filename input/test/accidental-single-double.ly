\header{
texidoc="
A sharp sign after a double sharp sign, as well as a flat sign
after a double flat sign is automatically prepended with a
natural sign.
";
}
\version "1.3.117";


thenotes =  \notes \relative cis' { \time 4/4;
gisis'4 gis gisis ges |
geses ges geses gis |
gisis g geses g |
gis g ges g |
\key a \major;
gisis4 gis gisis ges |
geses ges geses gis |
gisis g geses g |
gis g ges g |
}

\score { < \context Staff \thenotes
	\context NoteNames  {
		\property NoteNames.basicNoteNameProperties \override  #'no-spacing-rods  = ##f 
		\thenotes
	}
	>
}
