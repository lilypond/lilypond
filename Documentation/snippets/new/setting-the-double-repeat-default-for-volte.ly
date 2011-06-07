\version "2.14.0"
\header {
  lsrtags = "repeats"
  texidoc = "There are three different styles of double repeats for
volte, that can be set using @code{doubleRepeatType}."

  doctitle = "Setting the double repeat default for volte"
}


\relative c'' {
  \repeat volta 1 { c1 }
  \set Score.doubleRepeatType = #":|:"
  \repeat volta 1 { c1 }
  \set Score.doubleRepeatType = #":|.|:"
  \repeat volta 1 { c1 }
  \set Score.doubleRepeatType = #":|.:"
  \repeat volta 1 { c1 }
}
