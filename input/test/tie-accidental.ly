\header{
texidoc="
When tieing notes with accidentals across a bar boundary, the accidental
must not be drawn on the note in the new bar.  Instead, the next note of
the same pitch in this bar should always show the accidental (even if
it's natural).  Slurring a accidentaled note to a natural one across bar
boundaries should be explicit.

Pitches can be verified by printing them  with the @code{NoteNames} context.
";
}

thenotes = \notes \relative cis' { \time 4/4;
g'2 g ~ |
g g4 gis |
gis2 gis ~ |
gis4 gis8 ~ gis g4 gis |
g2 gis ~ |
gis g4 gis |
g2 gis( |
)g! gis4 gis |
\break
\key a \major;
gis2 gis ~ |
gis4 gis8 ~ gis g4 gis |
gis2 g ~ |
g4 gis8 ~ gis g4 gis |
g2 gis ~ |
gis g4 gis |
g2 gis( |
)g! gis4 gis | 
}

\score { < \context Staff \thenotes
	\context NoteNames \thenotes
	>
}
