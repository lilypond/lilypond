\version "2.12.0"
#(set-default-paper-size "a6")

\header {
  texidoc = "The space between systems can be limited when there is too
much space left on the page by setting @code{page-limit-inter-system-space}."
}

\book {
  \paper {
    page-limit-inter-system-space = ##t
    page-limit-inter-system-space-factor = 1.4

    ragged-last-bottom = ##f

    oddFooterMarkup = \markup "page bottom"
    evenFooterMarkup = \markup "page bottom"
    oddHeaderMarkup = \markup \fill-line {
      "page top" \fromproperty #'page:page-number-string }
    evenHeaderMarkup = \markup \fill-line {
      "page top" \fromproperty #'page:page-number-string }
  }
  \new Staff << \repeat unfold 9 { g'4 g' g' g' \break }
                { s1*2 \pageBreak
                  s1*3 \pageBreak } >>
}
