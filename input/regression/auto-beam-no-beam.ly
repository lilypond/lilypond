\header {

    texidoc = "The autobeamer may be switched off for a single note
    with @code{\\noBeam}."


    }

\version "2.19.21"
\layout {
    ragged-right = ##t
}

\relative { c'8 c-\noBeam c c }
