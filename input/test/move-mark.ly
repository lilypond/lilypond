\version "1.7.18"

\score{
\context Staff \notes\relative c''{
	c1 
	\context Score {
		\outputproperty #(make-type-checker 'Mark) 
		#'extra-offset = #'(-1 . 4)
	}
	\mark A
	d
	\mark \default
	e
}
\paper{
	raggedright = ##t
	\translator {
		\ScoreContext
		\consists "Mark_engraver"
	}
}
}
%% new-chords-done %%
