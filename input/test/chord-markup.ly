\header{
    texidoc = "chord/markup test"
}

\paper{ linewidth = -1.0\mm }

\score{
    <
	\context Staff \notes \relative c'' {
	    c^#'(columns "foe" ((raise . 3) "bar"))
	    c
	    c^#'(columns "foe" (super "12") (sub "3 4"))
	    c

	    %% broken for now
	    %%  c^#`(columns (lines "" ";" "") (lines "1" (bold "2") "3"))
	    c^#`(columns (lines "" ";" "") (lines "1" (columns (bold "2")) "3"))
		}
	\context ChordNames \chords {
	    c:7+.9-^3.5
	}
   >
}
