\score{
\context Staff \notes\relative c''<
	\context Voice = x {
		a4 
		r 
		\property Voice.maximumRestCount = #3
		r 
		\property Staff.maximumRestCount = #2
		r 
		\property Staff.maximumRestCount = #1
		r 
		r8 r8
		c
	}
	\context Voice = y {
		c4
		r
		r 
		r
		r
		r
		r
	}
	\context Voice = z {
		e4
		r
		r 
		r
		r
		r
		r
	}
	>
\paper{
}
\midi{
\tempo 1 = 60;
}
}
