\version "1.5.68"
\header{
    texidoc="Controlling beam positions."
}
\score { 
    \context Voice \notes\relative c {
	%% from upper staffline (position 4) to centre (position 0)
	\property Voice.Beam \override #'positions = #'(2 . 0)
	[c'8 c] 
	
	%% from center to one above centre (position 2)
	\property Voice.Beam \override #'positions = #'(0 . 1)
	[c c]
	
	%% normal beam-algorithm
	\property Voice.Beam \revert #'positions
	\property Voice.Beam \revert #'positions
	[c e] [e c]
  }
}
