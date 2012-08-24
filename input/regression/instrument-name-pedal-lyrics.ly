\version "2.16.0"

\header {
  texidoc = "@code{Dynamics} and @code{Lyrics} lines below
a @code{PianoStaff} do not affect the placement of the instrument name."
}

upper = \relative c'' {
  a4 b c d
}

lower = \relative c {
  \clef bass
  a2 c
}

\score { 
<<
  \new PianoStaff 
  <<
    \set PianoStaff.instrumentName = #"Piano"
    \new Staff = "Staff_pfUpper" << \upper >>
    \new Staff = "Staff_pfLower" << \lower >>
    \new Dynamics { s2\sustainOn s\sustainOff }
  >>

  \new PianoStaff <<
    \set PianoStaff.instrumentName = #"Piano"
    \new Staff = "Staff_pfUpper" << \upper >>
    \new Staff = "Staff_pfLower" << \lower >>
    \new Lyrics \lyricmode { la2 la2 }
  >>

  \new PianoStaff <<
    \set PianoStaff.instrumentName = #"Piano"
    \new Staff = "Staff_pfUpper" << \upper >>
    \new Staff = "Staff_pfLower" << \lower >>
  >>
>>
}
