\version "1.7.18"
\header{
    texidoc = "chord/markup test"
}

\paper{ raggedright = ##t }

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
   >
}
%% new-chords-done %%
