\version "1.5.68"

\score
{
  \context StaffGroup = a < 
    % this is broken until further notice -- see refman
    % \property Staff.StaffSymbol \override #'line-count = #4
    \context Staff \outputproperty #(make-type-checker 'staff-symbol-interface)
      #'line-count = #4
    \context PianoStaff = b <
      \context Staff = "c" \notes\relative c'' { b1 }
      \context Staff = "d" \notes\relative c'' { b1 }
    >
  >

  \paper {
  	indent=100.0\mm
  	linewidth=150.0\mm
    \translator
    {
      \StaffContext
      \consists Instrument_name_engraver
    }
  }
}
