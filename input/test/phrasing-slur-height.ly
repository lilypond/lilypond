\version "1.7.18"
\header{ texidoc="@cindex Phrasing Slur Height
Make PhrasingSlur higher, to avoid colission from other slurs. "
}

\score {
    \context Staff = melody \notes\relative c''{
	\property Staff.PhrasingSlur \override #'height-limit = #8.0
	c8 \( (d e f-) g ( a b c-)
	| c ( b a g-) f ( e d c-)-\)
    }
    \paper { raggedright = ##t }
}
%% new-chords-done %%
