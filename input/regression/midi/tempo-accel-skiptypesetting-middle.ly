\version "2.27.0"

\header {
  texidoc = "The @code{skipTypesetting} feature does not interfere with
rendering accelerando.

In this case, two skipped sections create apparent discontinuities in an
accelerando."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \fixed c' {
    \tempo 4 = 60
    \startGradualTempoChange \default
    \*2 \*16 c16
    \set Score.skipTypesetting = ##t
    \*2 \*16 cis16
    \set Score.skipTypesetting = ##f
    \*2 \*16 d16
    \set Score.skipTypesetting = ##t
    \*2 \*16 dis16
    \set Score.skipTypesetting = ##f
    \*2 \*16 e16
    \tempo 4 = 240
    f1
  }
  \midi { }
}
