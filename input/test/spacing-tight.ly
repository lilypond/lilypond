\header{
texidoc="
If there are accidentals in the music, we add space, but the space
between note and accidentals is less than between the notes with the
same value.  Clef changes also get extra space, but not as much as
barlines.

Even if a line is very tightly spaced, there will still be room
between prefatory matter and the following notes.  The space after the
prefatory is very rigid.  In contrast, the space before the barline
must stretch like the space within the measure.

Tight:
";
}
\score {
	\notes { \time 2/2; f''2 c'2 \time 2/2; }
	\paper { linewidth = 2.5 \cm;
	indent = 0.0;
	}
}
