\version "2.13.4"

\header {
  lsrtags = "expressive-marks, unfretted-strings"
  texidoc = "
A snap-pizzicato (also known as \"Bartok pizzicato\") is a \"strong
pizzicato where the string is plucked vertically by snapping and
rebounds off the fingerboard of the instrument\" (Wikipedia).  It is
denoted by a cicle with a vertical line going from the center upwards
outside the circle.
"
  doctitle = "Snap-pizzicato (\"Bartok pizzicato\")"
}

\relative c' {
  c4\snappizzicato
  <c' e g>4\snappizzicato
  <c' e g>4^\snappizzicato
  <c, e g>4_\snappizzicato
}
