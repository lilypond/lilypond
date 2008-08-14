\version "2.11.51"

\header {
  texidoc = "A table of contents is included using
@code{\\markuplines \\table-of-contents}. The toc items are added with
the @code{\\tocItem} command."
}

#(set-default-paper-size "a6")

\book {
  \markuplines \table-of-contents
  \pageBreak

  \tocItem \markup "The first score"
  \score {
    { 
      c'1 \pageBreak
      \mark "A" \tocItem \markup "Mark A"
      d'
    }
  }
  \pageBreak
  \tocItem \markup "The second score"
  \score {
    { e' }
    \header { piece = "Second score" }
  }
}