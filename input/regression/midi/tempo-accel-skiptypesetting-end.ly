\version "2.27.0"

\header {
  texidoc = "The @code{skipTypesetting} feature does not interfere with
rendering accelerando.

In this case, an accelerando ends in a skipped section.  The final tempo in
the MIDI file should be about 135 qpm."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \fixed c' {
    \tempo 4 = 30
    \startGradualTempoChange \default
    \*2 \*16 c16
    \set Score.skipTypesetting = ##t
    \*2 \*16 cis16
    \tempo 4 = 240
    cis1
  }
  \midi { }
}
