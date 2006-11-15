
\version "2.10.0"

\header { texidoc = "By setting @code{Staff.keySignature} directly,
key signatures can be set invidually per pitch.
"

      }
\layout {
    ragged-right = ##T
}
\relative c'
\new Staff {
    \set Staff.keySignature = #'(((1 .  2) . 1) ((0 . 3) . -1))
    f8 a c e
    \set Staff.keySignature = #'(((1 .  2) . -1) ((0 . 4) . 2))
    e a, g a
}

