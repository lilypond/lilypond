\version "2.4.0"

\header { texidoc = "
Fermatas over multimeasure rests are positioned as over normal rests.
" }


\layout {
    raggedright = ##t 
}
\relative c' {
    r1^\fermata R1^\fermataMarkup
}

