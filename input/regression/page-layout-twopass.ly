\header
{
  texidoc = "Page breaking details can be stored for later reference." 
}


\paper  {
  #(define write-page-layout #t)
}
bla = \new Staff {
  c1 c1
  \break
  \grace { c16 } c1\break
  \repeat unfold 5 \relative { c1 c1 c1 }
}

\book {
  \score {
    \bla
    \layout {
      #(define tweak-key "blabla")
    }
  }
}

tweakFileName = #(format "~a-page-layout.ly" (ly:parser-output-name parser))

#(newline)
#(ly:progress "Including file: ~a" tweakFileName)
\include \tweakFileName
