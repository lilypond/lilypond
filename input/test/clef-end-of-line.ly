\version "2.3.8"
\header {

    texidoc = "@cindex Clef End of Line
In these scales, the clef and key signature are shown at the end of the line.
" }

\score {
     \transpose c c' {
	\override Staff.Clef  #'break-visibility = #end-of-line-visible
	\override Staff.KeySignature  #'break-visibility = #end-of-line-visible
	\set Staff.explicitClefVisibility = #end-of-line-visible
	\set Staff.explicitKeySignatureVisibility = #end-of-line-visible

	% We want the time sig to take space, otherwise there is not
	% enough white at the start of the line.
	%
	
	\override Staff.TimeSignature  #'transparent = ##t
	\set Score.defaultBarType = #"empty"
	
	c1 d e f g a b c
	\key d \major
	\break

	% see above.
	\time 4/4
	
	d e fis g a b cis d 
	\key g \major
	\break
	\time 4/4    
} }

