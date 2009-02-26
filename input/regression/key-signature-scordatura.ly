
\version "2.12.0"

\header { texidoc = "By setting @code{Staff.keySignature} directly,
key signatures can be set invidually per pitch.
"

      }
\layout {
    ragged-right = ##T
}
\relative c'
\new Staff {
    \set Staff.keySignature = #`(((0 . 3) . ,FLAT) ((1 .  2) . ,SHARP))
    f8 a c e
    \set Staff.keySignature = #`(((0 . 4) . ,DOUBLE-SHARP) ((1 .  2) . ,FLAT))
    e a, g a
}

