
	
\version "1.3.96";

\score {

  \notes \context PianoStaff <
    \context Staff = treble    {
      \property PianoStaff.instrument = "Piano "
      \property Staff.instrument = "Right " { c''4 }}
    \context Staff = bass { \property Staff.instrument = "Left " \clef bass; c4 }>

\paper {
linewidth=-1.0;
\translator { \ScoreContext
	%textVerticalAlignment = #0
	}
%\translator { \StaffContext \consists "Instrument_name_engraver"; }
%\translator { \PianoStaffContext \consists "Instrument_name_engraver"; }
\translator { \StaffContext \consists "Instrument_name_engraver"; }
\translator { \PianoStaffContext \consists "Instrument_name_engraver"; }
}}

