\version "2.3.4"
\header {
    texidoc = "The meaning of @code{|} is stored in the
identifier @code{pipeSymbol}."
}

pipeSymbol = \bar "||"

\score {
      { c'2 | c'2 | }
}
    
