\version "2.1.23"

\header{texidoc = "@cindex Staff Bracket
Here's an example of staff brackets. "
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

