\version "2.3.16"

\header { texidoc = "
Fermatas over multimeasure rests are positioned as over normal rests.
" }


\paper {
    raggedright = ##t 
}
\relative c' {
    r1^\fermata R1^\fermataMarkup
}

