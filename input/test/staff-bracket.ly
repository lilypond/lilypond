
\score
{
  \context StaffGroup = a < 
    \context PianoStaff = b <
      \context Staff = "c" \notes\relative c'' { b1 }
      \context Staff = "d" \notes\relative c'' { b1 }
    >
  >

  \paper {
  	indent=100.0\mm;
  	linewidth=150.0\mm;
    \translator
    {
      \StaffContext
      \consists Instrument_name_engraver;
	numberOfStaffLines  = #1
      
    }
  }
}
