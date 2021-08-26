\version "2.21.0"

\header {
  texidoc = "Text gets kerned if the used font supports that."
}

\markup {
  \override #'((padding . 0)
               (thickness . 0.00001)
               (box-padding . 0.0))
  \column {
    \line {
      \pad-to-box #'(0 . 20) #'(0 . 2)
        \line { With kerning: }
      \override #'(font-size . 12) {
        \box { VAVAVA }
        \box { \dynamic { mp } }
      }
    }

    \line {
      \pad-to-box #'(0 . 20) #'(0 . 2)
        \line { Without kerning: }
      \override #'((font-size . 12)
                   (word-space . 0.0)) {
        \line { \box V \box A \box V \box A \box V \box A }
        \line { \dynamic { \box m \box p } }
      }
    }
  }
}
