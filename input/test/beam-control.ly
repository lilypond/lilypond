
\version "2.1.22"
\header{
    texidoc="@cindex Beam Position Control

Beam positions may be controlled manually, by setting @code{positions} in the @code{Beam} grob.

" }
\score { 
    \context Voice \notes\relative c {
	%% from upper staffline (position 4) to centre (position 0)
	\override Beam  #'positions = #'(2 . 0)
	 c'8[ c] 
	
	%% from center to one above centre (position 2)
	\override Beam  #'positions = #'(0 . 1)
	 c[ c]
	
	%% normal beam-algorithm
	\revert Beam #'positions
	\revert Beam #'positions
	 c[ e]  e[ c]
  }
\paper{raggedright = ##t}
}

