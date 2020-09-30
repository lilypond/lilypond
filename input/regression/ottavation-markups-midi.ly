\version "2.21.0"

#(ly:set-option 'warning-as-error #t)

\header {
  texidoc = "LilyPond should warn about missing ottavation
markups only if there is a list of ottavation markups defined.
This is not the case for MIDI performers, so do not output a
warning."
}

\score {
 \fixed c' { \ottava #1 c1 }
 \midi { }
}
