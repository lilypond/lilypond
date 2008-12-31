\header {

    texidoc = "The attachment point for strongly sloped slurs is
shifted horizontally slightly. Without this correction, slurs will
point into one note head, and point over another note head."
 
}
\layout {
    ragged-right = ##t
}
\version "2.12.0"

\relative c'' {
    \time 2/4
    g8( f)
    a( f)
    b( f)
    c'( f,)

    g16( f)
    a( f)
    b( f)
    c'16( f,)
    

}
