\version "2.1.29"
\header { texidoc = "There are both long and short intstrument names.
Engraving instrument names should not be confused by the
multimeasure rests. " }
% (there was an interesting bug in 1.3.98)
\score{
	\context Staff <<
	      \set Staff.instrument = "instrument"
	      \set Staff.instr = "instr"
	      \notes{c''1 \break R1 }
	    >>
	 \paper {
	    \context { \StaffContext
	    \consists Instrument_name_engraver

	    }
	  }
	\paper { raggedright = ##t }
	}


