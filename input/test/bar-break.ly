\score
{
  \context StaffGroup = a < 
    \context PianoStaff = b <
      \context Staff = "c" \notes\relative c'' { b1 \break b }
      \context Staff = "d" \notes\relative c'' { b1 \break b }
    >
  >

  \paper {
  	indent=100.0\mm;
  	linewidth=150.0\mm;
    \translator
    {
      \StaffContext
        \consists "Staff_margin_engraver";
	numberOfStaffLines  = #1
        marginScriptPadding = #10  % urg: this is in PT
	instrument = #"Foo"
	instr = #"Foo"
    }
  }
}
