\version "2.25.24"

#(set-global-staff-size 40)

\layout {
  \context {
    \Staff
    \remove Clef_engraver
    \remove Time_signature_engraver
  }
}

\fixed c' {
  \time 4/1

  \once \override #(list testGrobName 'direction) = #RIGHT
  <f a c'>4\testCommand
  <f a c'>4\testCommand

  \once \override #(list testGrobName 'direction) = #RIGHT
  <f a c'>2\testCommand
  <f a c'>2\testCommand
}
