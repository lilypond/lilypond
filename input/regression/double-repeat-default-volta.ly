\version "2.19.21"

\header {
  texidoc = "For volte, the style of double repeats can be set
  using @code{doubleRepeatType}."
}

\relative {
  \repeat volta 1 {
    c'1
  }
  \mark "default"
  \repeat volta 1 {
    c1
  }
  \mark "\":|.|:\""
  \set Score.doubleRepeatType = ":|.|:"
  \repeat volta 1 {
    c1
  }
  \mark "\":|.:\""
  \set Score.doubleRepeatType = ":|.:"
  \repeat volta 1 {
  c1
  }
}
