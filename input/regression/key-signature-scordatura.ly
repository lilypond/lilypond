
\version "2.11.51"

\header { texidoc = "By setting @code{Staff.keySignature} directly,
key signatures can be set invidually per pitch.
"

      }
\layout {
    ragged-right = ##T
}
\relative c'
\new Staff {
    \set Staff.keySignature = #`(((1 .  2) . ,SHARP) ((0 . 3) . ,FLAT))
    f8 a c e
    \set Staff.keySignature = #`(((1 .  2) . ,FLAT) ((0 . 4) . ,DOUBLE-SHARP))
    e a, g a
}

