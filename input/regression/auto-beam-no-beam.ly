\header {

    texidoc = "The autobeamer may be switched off for a single note
    with @code{\\noBeam}."


    }

\version "2.16.0"
\layout {
    ragged-right = ##t
}

\relative c' { c8 c-\noBeam c c }
