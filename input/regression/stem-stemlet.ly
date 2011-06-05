\header {

    texidoc = "Stemlets are small stems under beams over rests.  Their
length can be set with @code{stemlet-length}."

}

\version "2.14.0"
\layout { ragged-right = ##t }

\relative c' {
    \override Stem #'stemlet-length = #0.75
    c8[ r8 c16 r16 c8]
    c4
}
