\version "2.23.3"

\header {
  texidoc = "The @code{text} property of an @code{OttavaBracket} grob
may be overridden."
}

{
  \once \override Staff.OttavaBracket.text = "Test OK"
  \ottava 1
  c1
  \alterBroken text #'("First text" "Second text") Staff.OttavaBracket
  \ottava -1
  c1
  \break
  c1
}
