#(ly:set-option 'old-relative)
\version "1.9.1"
% possible rename to rest-foo
\header {
    texidoc = "@cindex Rest Collision Count
Control the number of rests in a collision with maximum-rest-count."
}

\score{
\context Staff \notes\relative c''<
	\context Voice = x {
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
	\paper{ raggedright = ##t }
}

