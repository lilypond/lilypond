\version "2.1.22"
% possible rename to rest-foo
\header {
    texidoc = "@cindex Rest Collision Count
Control the number of rests in a collision with maximum-rest-count."
}

\score{
\context Staff \notes\relative c''<<
	\new Voice {
		a4 
		r 
		\override Staff.RestCollision  #'maximum-rest-count = #1
		r 
		\override Staff.RestCollision  #'maximum-rest-count = #2
		r 
		\override Staff.RestCollision  #'maximum-rest-count = #3
		r 
		r8 r8
		c
	}
	\new Voice {
		c4
		r
		r 
		r
		r
		r
		r
	}
	\new Voice {
		e4
		r
		r 
		r
		r
		r
		r
	}
	>>
	\paper{ raggedright = ##t }
}

