\version "1.9.8"

\header {
    texidoc = "Ledger lines are shortened when there are accidentals."
}

\score { \notes
{
 c'!4 cis'! cis' <cis'! gis'>
}
	 \paper { raggedright = ##t}
}
