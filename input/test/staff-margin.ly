
	
\version "1.3.5";

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
\translator { \StaffContext \consists "Staff_margin_engraver"; }
\translator { \PianoStaffContext \consists "Staff_margin_engraver"; }
}}

