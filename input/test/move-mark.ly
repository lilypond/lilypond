\version "1.7.18"
% possible rename to scheme- or something like that.  -gp
\header { texidoc = "@cindex Scheme Move Mark
You can move objects around with scheme.  This example shows how to
move marks around. " }

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
