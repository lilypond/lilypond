\version "2.4.0"
\header {

    texidoc = "There are both long and short instrument names.
Engraving instrument names should not be confused by the
multimeasure rests. "

}

\score{
    \context Staff <<
	\set Staff.instrument = "instrument"
	\set Staff.instr = "instr"
	{c''1 \break R1 }
    >>
    \layout {
	raggedright = ##t
	\context {
	    \Staff
	    \consists Instrument_name_engraver

	}
    }
}



