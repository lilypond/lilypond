\version "1.3.146"

\header{
texidoc="
Grace notes are typeset as an encapsulated piece of music. You can
have beams, notes, chords, stems etc. within a @code{\grace} section.
Slurs that start within a grace section, but aren't ended are attached
to the next normal note.  Grace notes have zero duration.  If there
are tuplets, the grace notes won't be under the brace.  Grace notes
can have accidentals, but they are (currently) spaced at a fixed
distance.  Grace notes (of course) come before the accidentals of the
main note.  Grace notes can also be positioned after the main note.

Grace notes without beams should have a slash, if @code{flagStyle} is
not set.  Main note scripts don't end up on the grace note.

"
}

\score {\notes \context Voice = VA \relative c'' {
	\grace b8 c4-\fermata
	\grace { [c32 cis32] } gis4
	\grace { [cis32 dis32] } e4
	\grace { [c32 d] }\times 2/3 { [c8 c c] }
	 \grace { [b32 ( c32] } ) c4
	\grace  <c16 d16> [c8 c8]
%	\grace  c16 [c8 c8]
        %% broken?
	%%\grace  { \property Grace.graceAlignPosition = \right c16} c4
	c4 \grace  { c16 } c4
}
\paper {linewidth = -1.}
\midi{ }
}
