\version "2.23.11"

\header {
  texidoc = "The performance name of the MIDI file can contain
Unicode characters.  They are encoded in UTF8.

Note: due to the way the regression test infrastructure currently
works, any breakage in this test will not be noticed automatically."
}

\score {
  \header {
    title = "Łïłîρøñđ"
  }
  { c' }
  \midi { }
}
