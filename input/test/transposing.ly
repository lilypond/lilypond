\header {
texidoc = " the transposing property leaves output invariant, but has effect on MIDI."

}
\version "1.5.68"

\score { 
  \context Voice \notes\relative c {
    % btw: this is not how transposing is done in lilypond
	% this is a transposing performer, i.e. for midi-output only
	\property Staff.transposing = #0 c
	\property Staff.transposing = #2 c
	\property Staff.transposing = #4 c
	\property Staff.transposing = #5 c
	\property Staff.transposing = #7 c
	\property Staff.transposing = #9 c
	\property Staff.transposing = #11 c
	\property Staff.transposing = #12 c
	
  }
  \paper { }  
  \midi { }
}
