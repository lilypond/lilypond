\version "2.19.9"

\header {
  texidoc = "Alignment of lyrics, dynamics, textscripts and articulations
attached to chords with suspended notes doesn't depend on input order.
All these items are aligned on the \"main\" notehead (the one at the
end of the stem)."
}

<<
  \new Staff {
    <b' c''>2 s
    <b' c''>\f s
    <b' c''>^"Text" s
    <b' c''>-! s
  }
  \addlyrics { blah }
  \new Staff {
    <c'' b'>2 s
    <c'' b'>\f s
    <c'' b'>^"Text" s
    <c'' b'>-! s
  }
  \addlyrics { blah }
>>
