\version "1.7.18"
% similiar to beam-position, but keep this one.
\header{
    texidoc="@cindex Beam Position Control
Controlling beam positions.
" }
\score { 
    \context Voice \notes\relative c {
	%% from upper staffline (position 4) to centre (position 0)
	\property Voice.Beam \override #'positions = #'(2 . 0)
	 c'8-[ c] 
	
	%% from center to one above centre (position 2)
	\property Voice.Beam \override #'positions = #'(0 . 1)
	 c-[ c]
	
	%% normal beam-algorithm
	\property Voice.Beam \revert #'positions
	\property Voice.Beam \revert #'positions
	 c-[ e]  e-[ c]
  }
\paper{raggedright = ##t}
}

