
\version "2.19.21"
\header {

  texidoc = "Tuplets may contain rests. "

}


\context Voice  \relative {
  \time 2/4
  \tuplet 3/2 { r c, c''' }
  \tuplet 3/2 { r c c  }
  \tuplet 3/2 { r c r }
  \tuplet 3/2 { r r r }
  \tuplet 3/2 { r c e }
  \tuplet 3/2 { c r e }
  \tuplet 3/2 { r c g }
  \tuplet 3/2 { c r g }
}



