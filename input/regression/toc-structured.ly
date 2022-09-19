\version "2.21.2"

\header {
  texidoc = "TOC labels can be explicitly specified,
and structured hierarchically; they appear in PDF
bookmarks as well (the `table of contents' panel
in PDF viewers).  PDF bookmarks are reordered so as
to not `go back in time'."
}

#(set-default-paper-size "a8" 'landscape)

\book {
  \header { tagline = ##f }
  \markuplist \table-of-contents
  \pageBreak

  \tocItem \markup \underline "Introduction"
  \markup \bold \fill-line { "Hello World." }
  \pageBreak
  \tocItem parentI \markup "First-level I."

  \score {
    {
      c'1 \pageBreak
      \tocItem parentI.pIchildI \markup "Second level I. a"
      d'
      \mark \default \tocItem pIchildI.pIcIgrandchildI \markup "Third level I. a, 1"
      e'
      \pageBreak
    }
  }
  \pageBreak
  \tocItem parentII \markup "First-level II."
  \score {
    <<
      \new Devnull {
        R R \pageBreak
        \tocItem parentII.pIIchildI \markup "The end"
        R \bar "|."
      }
      %% This will appear in the TOC after the previous entry,
      %% but before it in the PDF outline.
      { e' \tocItem parentII.pIIchildII \markup "Before the end" s f' }
    >>
  }
}
