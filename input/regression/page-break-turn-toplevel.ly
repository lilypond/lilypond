\version "2.16.0"

\header {
  texidoc = "Page breaking and page turning commands (@code{\\pageBreak}, 
@code{\\noPageBreak}, etc), can be used at top level."
}

#(set-default-paper-size "a6")
\paper{ #(define page-breaking ly:page-turn-breaking) }

{ c'1 \break c'_\markup \typewriter "\\allowPageTurn" }
\allowPageTurn
{ e'1 \break e'_\markup \typewriter "\\pageBreak \\noPageTurn" }
\pageBreak
\noPageTurn
{ g'1 \break g' }
