
%{

this combines instrument names and multimeasure rests (there was an
interesting bug in 1.3.98)

%}

\version "1.3.117";
\score{
	\context Staff <
	      \property Staff.instrument = "instrument"
	      \property Staff.instr = "instr"
	      \notes{c''1 \break R1 }
	    >
	 \paper {
	    \translator { \StaffContext
	    \consists Instrument_name_engraver;

	    }
	  }
	}

