%
% when part-combining, rests should collide as in normal case
%
\score {
  \notes <
    \context Staff <
       \context Voice=one \relative c''{
	 d4 d d d
	 d d d d
       } 
       \context Voice=two {
	 r1
	 R
       } 
    >
    \context Staff = Viole <
	\context Voice=one \partcombine Voice
		\context Thread=one \relative c''{ d4 d d d  d d d d }
		\context Thread=two { r1 R1 }
    >
  >
  \paper {
  }
}