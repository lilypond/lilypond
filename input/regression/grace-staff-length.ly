#(ly:set-option 'old-relative)
\version "1.9.8"
\header{
texidoc = "Stripped version of trip.ly.  Staffs should be of correct length."
}

    \paper { raggedright= ##t }

\score{
  \context PianoStaff \notes \relative c'' <<
    \new Staff {
       r1
       r1
       \bar "|."
    }
    \new Staff {
      r1
      \context Staff {
	\grace { c16 } c1
      }
    }
  >> 
  \paper { }
}

