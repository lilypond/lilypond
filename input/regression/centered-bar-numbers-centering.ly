\version "2.23.3"

\header {
  texidoc = "The centering of measure-centered bar numbers does not take
prefatory material (such as clefs and time signatures) into account in
the extent of the measure.  This may be overridden by the user."
}

\new Score \with {
  centerBarNumbers = ##t
} {
  c'1
  \key cis \major
  \clef bass
  \time 3/4
  c'4 4 4
  \override Score.CenteredBarNumber.spacing-pair
    = #'(staff-bar . staff-bar)
  \key ces \major
  \time 4/4
  \clef treble
  c'4 4 4 4
}