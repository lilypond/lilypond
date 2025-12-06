\version "2.23.14"

\header {
  texidoc = "Each clef has its own accidental placing rules, which can be
adjusted using @code{sharp-positions} and @code{flat-positions}."
}


#(set-global-staff-size 16)

music = \relative {
  % \clef french % same as octavated bass
  \clef violin
  \key cis \major cis'1 |
  \key ces \major ces |

  \clef soprano
  \key cis \major cis | \break
  \key ces \major ces |

  \clef mezzosoprano
  \key cis \major cis |
  \key ces \major ces | \break

  \clef alto
  \key cis \major cis |
  \key ces \major ces |

  \clef tenor
  \key cis \major cis | \break
  \key ces \major ces |

  \clef baritone
  \key cis \major cis |
  \key ces \major ces | \break

  \clef bass
  \key cis \major cis |
  \key ces \major ces \bar "||" \break
}

{
  \textMark \markup \column {
    \line { \typewriter "sharp-positions = #'(2 3 4 2 1 2 1)" (default) }
    \line { \typewriter "flat-positions = #'(4 5 4 2 3 2 3)" (default) } }
  \music
}

{
  \override Staff.KeySignature.sharp-positions = #'(6 0 1 2 3 4 5)
  \override Staff.KeyCancellation.sharp-positions = #'(6 0 1 2 3 4 5)
  \override Staff.KeySignature.flat-positions = #'((-5 . 5))
  \override Staff.KeyCancellation.flat-positions = #'((-5 . 5))

  \textMark \markup \column {
    { \typewriter "sharp-positions = #'(6 0 1 2 3 4 5)" }
    { \typewriter "flat-positions = #'((-5 . 5))" } }
  \music
}
