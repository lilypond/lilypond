#(ly:set-option 'old-relative)
\version "1.9.8"
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
		\property Staff. RestCollision \set #'maximum-rest-count = #1
		r 
		\property Staff. RestCollision \set #'maximum-rest-count = #2
		r 
		\property Staff. RestCollision \set #'maximum-rest-count = #3
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

