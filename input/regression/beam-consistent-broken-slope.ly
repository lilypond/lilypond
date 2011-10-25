
\version "2.15.15"

\header {
  texidoc = "The @code{consistent-broken-slope} property of @code{Beam}
allows for slopes to be almost consistent across line breaks.  Almost
because quanting can still cause minor differences between beams slopes.
"
}

\relative c' {
  \override Beam #'breakable = ##t
  a8[ b c d e f g \bar "" \break f e d c b a]
}

\relative c' {
  \override Beam #'breakable = ##t
  \override Beam #'consistent-broken-slope = ##t
  a8[ b c d e f g \bar "" \break f e d c b a]
}

\relative c' {
  \override Beam #'breakable = ##t
  a8[ b c d e f \bar "" \break a c e g b]
}

\relative c' {
  \override Beam #'breakable = ##t
  \override Beam #'consistent-broken-slope = ##t
  a8[ b c d e f \bar "" \break a c e g b]
}
