#(ly:set-option 'old-relative)
\version "1.9.1"

\header{texidoc = "@cindex Staff Bracket
Here's an example of staff brackets. "
}

\score
{
  \context StaffGroup = "a" \notes \relative c'' <
      \context Staff = "a" {  b1 }
    \context PianoStaff = "b" <
      \context Staff = "c" \notes { b1 }
      \context Staff = "d" \notes { b1 }
    > 
  >

  \paper {
    \translator
    {
      \StaffContext
      \consists Instrument_name_engraver
    }
    raggedright=##t
  }
}

