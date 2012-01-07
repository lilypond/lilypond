\version "2.15.24"

\header {
  texidoc = "The @code{barNumberVisibility} property controls at what
intervals bar numbers are printed.
"
}

music = \relative c' {
  \repeat unfold 3 { c2 \bar "" \break \repeat unfold 5 c2 }
}

{
  \set Score.barNumberVisibility = #first-bar-number-invisible
  \music
}

{
  \set Score.barNumberVisibility = #all-bar-numbers-visible
  \music
}

{
  \set Score.barNumberVisibility = #(every-nth-bar-number-visible 3)
  \music
}

{
  \set Score.barNumberVisibility = #(modulo-bar-number-visible 3 2)
  \music
}

{
  \set Score.barNumberVisibility = #first-bar-number-invisible-save-broken-bars
  \music
}

{
  \set Score.barNumberVisibility = #first-bar-number-invisible-and-no-parenthesized-bar-numbers
  \music
}
