
\version "2.19.21"

\header { texidoc = "By setting @code{Staff.keyAlterations} directly,
key signatures can be set invidually per pitch.
"

      }
\layout {
    ragged-right = ##T
}
\relative
\new Staff {
    \set Staff.keyAlterations = #`(((0 . 3) . ,FLAT) ((1 .  2) . ,SHARP))
    f'8 a c e
    \set Staff.keyAlterations = #`(((0 . 4) . ,DOUBLE-SHARP) ((1 .  2) . ,FLAT))
    e a, g a
}

