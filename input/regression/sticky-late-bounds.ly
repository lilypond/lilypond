\version "2.23.4"

\header {
  texidoc = "Sticky spanners also work when the host's bounds
are not set immediately, such as with a hairpin ending on a skip."
}

#(set-default-paper-size "a6")

\book {
  { s8\footnote #'(-2 . -2) "footnote"\> s8\! }
  \new Voice \with {
    \consists Balloon_engraver
  }
  { \balloonGrobText Hairpin #'(-2 . -2) "annotation" s8\> s8\! }
  { s8\parenthesize\> s8\! }
}
