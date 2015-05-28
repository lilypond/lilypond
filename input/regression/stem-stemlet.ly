\header {

    texidoc = "Stemlets are small stems under beams over rests.  Their
length can be set with @code{stemlet-length}."

}

\version "2.19.21"
\layout { ragged-right = ##t }

\relative {
    \override Stem.stemlet-length = #0.75
    c'8[ r8 c16 r16 c8]
    c4
}
