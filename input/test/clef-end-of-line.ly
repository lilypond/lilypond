\header {

    texidoc = "Scales, but with clef and key signature at the end of the line."

}

\score {
    \notes \transpose c'' {
	\property Staff.Clef \set #'break-visibility = #end-of-line-visible
	\property Staff.KeySignature \set #'break-visibility = #end-of-line-visible
	\property Staff.explicitClefVisibility = #end-of-line-visible
	\property Staff.explicitKeySignatureVisibility = #end-of-line-visible

	% We want the time sig to take space, otherwise there is not
	% enough white at the start of the line.
	%
	
	\property Staff.TimeSignature \set #'transparent = ##t
	\property Score.defaultBarType = #"empty"
	
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
