\version "1.3.146"

\score { 
  \context Voice \notes\relative c {
    % beam-control.fly
	
	% from upper staffline (position 4) to centre (position 0)
	\property Voice.beamVerticalPosition = #4
	\property Voice.beamHeight = #-4
	[c'8 c] 
	
	% from center to one above centre (position 2)
	\property Voice.beamVerticalPosition = #0
	\property Voice.beamHeight = #2
	[c c]
	
	% normal beam-algorithm
	\property Voice.beamHeight = ##f
	\property Voice.beamVerticalPosition = ##f
	[c e] [e c]
	
  }
  \paper { }  
  \midi { }
}
