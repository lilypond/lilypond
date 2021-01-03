\version "2.23.0"

\header {
  texidoc = "@code{numbers-with-letters} bar numbering resets at the
end of the repeat even if the repeat ends where no bar number is
visible."
}

\new Score \with {
  alternativeNumberingStyle = #'numbers-with-letters
  \override BarNumber.break-visibility = #all-visible
} \fixed c' {
  \repeat volta 2 { c1 } \alternative { d1 e2 } f2 |
}
