\version "2.1.26"

\header{texidoc = "@cindex Staff Bracket
In a music with piano accompaniment, staff brackets, like the one in 
this example can be used. "
}

\score
{
  \new StaffGroup \notes \relative c'' <<
      \new Staff {  b1 }
    \new PianoStaff <<
      \new Staff \notes { b1 }
      \new Staff \notes { b1 }
    >> 
  >>

  \paper {
    \translator
    {
      \StaffContext
      \consists Instrument_name_engraver
    }
    raggedright=##t
  }
}

