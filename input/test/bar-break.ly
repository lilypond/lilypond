\score
{
  \context StaffGroup = a < 
    \context PianoStaff = b <
      \context Staff = "c" \notes\relative c'' { b4 b \bar "empty"; \break b b }
      \context Staff = "d" \notes\relative c'' { b4 b b b }
    >
  >

  \paper {
  	indent=100.0\mm;
  	linewidth=150.0\mm;
    \translator {
      \StaffContext
        \consists "Instrument_name_engraver";
	numberOfStaffLines  = #1
        marginScriptPadding = #30  % urg: this is in PT
	instrument = #"Foo"
	instr = #"Bar"
    }
    \translator {
      \StaffGroupContext
        \consists "Instrument_name_engraver";
        marginScriptPadding = #10  % urg: this is in PT
	instrument = #" \n \n \n \n \n \n \n \n \n \nPiano\n(For rehearsal only)"
      }
  }
}
