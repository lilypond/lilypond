%hairy grace stuff:

\score{
	\context Staff=foo \notes\relative c''{
		% two auto beams 
		d4 \grace c8 d8 \grace { d16 c16 } d8 c2
		\property Voice.verticalDirection = 1
		% colliding beams
		d4 \grace c8 d8 \grace { d16 c16 } d8 c2
		\property Voice.verticalDirection = 0
		% leger lines
		d,,4 \grace c8 d8 \grace { d16 c16 } d8 c2
	}
}
