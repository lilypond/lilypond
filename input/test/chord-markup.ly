\version "1.7.6"
\header{
    texidoc = "chord/markup test"
}

\paper{ linewidth = -1.0\mm }

\score{
    <
	\context Staff \notes \relative c'' {
	    c^\markup {  "foe" \raise #3 "bar" }
	    c
	    c^\markup { "foe" \super "12" \sub { "3 4" } }
	    c

	    %% broken for now
	    c^\markup { \column << "" ";" "" >>
			\column <<  "1"  { \bold "2" "3" } >> }
		}
	\context ChordNames \chords {
	    c:7+.9-^3.5
	}
   >
}
%% new-chords-done %%
