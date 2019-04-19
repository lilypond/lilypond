\version "2.21.0"

\header { texidoc = "
Fermatas over multimeasure rests are positioned as over normal rests.
" }


\layout {
    ragged-right = ##t 
}
\relative c' {
    r1^\fermata R1^\fermata
}

