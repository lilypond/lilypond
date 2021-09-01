\version "2.23.4"

\header {

  texidoc = "Page-headers and -footers.  All headers and footers
  should be printed on their specified page. "

}

\paper {
  ragged-last-bottom = ##f

  oddHeaderMarkup = \markup  {
    \override #'(baseline-skip . 2.5)
    \center-column {
      \box \fill-line { \teeny " " " " }
      \if \on-first-page
        "first-page-header-text"
      \unless \on-first-page
        \fromproperty #'page:page-number-string
      \if \on-page #2 "page-2-header-text"
      \if \on-last-page "last-page-header-text"
    }
  }

  evenHeaderMarkup = \oddHeaderMarkup

  oddFooterMarkup = \markup \fill-line {
    \override #'(baseline-skip . 1)
    \center-column {
	\if \on-first-page "first-page-footer-text"
	\if \on-last-page "last-page-footer-text"
	\if \on-page #2 "page-2-footer-text"
	\box \fill-line { \teeny " " " " }
    }
  }
}

#(set-default-paper-size "a6" 'portrait)

\book {
  \score {
    \new Staff \relative {
      \repeat unfold 18 { a b c d \break }
    }
  }
}
