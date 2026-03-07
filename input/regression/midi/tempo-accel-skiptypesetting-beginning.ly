\version "2.27.0"

\header {
  texidoc = "The @code{skipTypesetting} feature does not interfere with
rendering accelerando.

In this case, an accelerando begins in a skipped section.  The initial tempo in
the MIDI file should be about 156 qpm."
}

#(ly:set-option 'warning-as-error #t)

\score {
  \fixed c' {
    \set Score.skipTypesetting = ##t
    \*2 \*16 cis16
    \tempo 4 = 72
    \startGradualTempoChange \default
    \*2 \*16 cis16
    \set Score.skipTypesetting = ##f
    \*2 \*16 c16
    \tempo 4 = 240
    c1
  }
  \midi {
    \context {
      \Voice
      \remove Dynamic_performer  % TODO: trouble with skipping
    }
  }
}
