\version "2.13.4"

\header {
  lsrtags = "fretted-strings"
  texidoc = "
To add fingerings to tablatures, use a combination of @code{\\markup}
and @code{\\finger}.
"
  doctitle = "Adding fingerings to tablatures"
}

one = \markup { \finger 1 }
two = \markup { \finger 2 }
threeTwo = \markup {
  \override #'(baseline-skip . 2)
  \column {
    \finger 3
    \finger 2
  }
}
threeFour = \markup {
  \override #'(baseline-skip . 2)
  \column {
    \finger 3
    \finger 4
  }
}

\score {
  \new TabStaff {
    \tabFullNotation
    \stemUp
    e8\4^\one b\2 <e, g\3 e'\1>^>[ b\2 e\4]
    <a\3 fis'\1>^>^\threeTwo[ b\2 e\4]
  }
}

