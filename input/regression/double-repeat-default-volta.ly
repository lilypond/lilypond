\version "2.11.60"

\header {
  texidoc = "For volte, the style of double repeats can be set
  using @code{doubleRepeatType}."
}

\relative c' {
  \repeat volta 1 {
    c1
  }
  \mark "default"
  \repeat volta 1 {
    c1
  }
  \mark "\":|.|:\""
  \set Score.doubleRepeatType = #":|.|:"
  \repeat volta 1 {
    c1
  }
  \mark "\":|.:\""
  \set Score.doubleRepeatType = #":|.:"
  \repeat volta 1 {
  c1
  }
}
