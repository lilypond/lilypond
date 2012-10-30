\version "2.17.6"

\header {
  texidoc = "Fingerings work correctly with cross-staff beams."
}

music = \relative c {
  \change Staff = "down"
  c8 e g-1
  \change Staff = "up"
  e'-2 g-3 c-5
  \change Staff = "down"
  e,-3[ g,-1]
}

\score {
  \new PianoStaff  <<
    \new Staff="up" \new Voice { 
      \stemUp \music }
    \new Staff="down" \new Voice { 
      \clef bass  \stemDown 
      \override Fingering.direction = #down 
      \transpose c c, \music }
  >>
}
