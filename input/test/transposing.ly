\version "1.3.146"

\score { 
  \context Voice \notes\relative c {
    % btw: this is not how transposing is done in mudela,
	% this is a transposing performer, i.e. for midi-output only
	\property Staff.transposing = 0 c
	\property Staff.transposing = 2 c
	\property Staff.transposing = 4 c
	\property Staff.transposing = 5 c
	\property Staff.transposing = 7 c
	\property Staff.transposing = 9 c
	\property Staff.transposing = 11 c
	\property Staff.transposing = 12 c
	
  }
  \paper { }  
  \midi { }
}
