\version "2.3.0"
\header {
    texidoc = "The meaning of @code{|} is stored in the
identifier @code{pipeSymbol}."
}

pipeSymbol = \bar "||"

\score {
    \notes  { c'2 | c'2 | }
}
    
