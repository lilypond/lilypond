\version "2.3.16"
\header {
texidoc = "Grace code should not be confused by nested sequential musics, containing grace notes; practically speaking, this means that the end-bar and measure bar coincide in this example." 

}
    \paper { raggedright= ##t }


\score{
 

{ c''2 { \grace  b'16  c''2 }  \bar "|." }
  \paper {
}
 }


