\version "2.19.16"

\header {
  texidoc = "An ossia staff without a @code{Time_signature_engraver}
stops right at the bar line."
}

\relative c'' {
  \time 2/4
  c2 |

  <<
    { c2 }
    \new Staff \with { \remove "Time_signature_engraver" } { c2 }
  >> |

  \time 3/4
  c2. |
}
