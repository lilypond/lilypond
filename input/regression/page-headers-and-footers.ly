\version "2.16.0"

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
      \on-the-fly #first-page "first-page-header-text"
      \on-the-fly #not-first-page \fromproperty #'page:page-number-string
      \on-the-fly #(on-page 2) "page-2-header-text"
      \on-the-fly #last-page "last-page-header-text"
    }
  }

  evenHeaderMarkup = \oddHeaderMarkup

  oddFooterMarkup = \markup \fill-line {
    \override #'(baseline-skip . 1)
    \center-column {
	\on-the-fly #first-page "first-page-footer-text"
	\on-the-fly #last-page "last-page-footer-text"
	\on-the-fly #(on-page 2) "page-2-footer-text"
	\box \fill-line { \teeny " " " " }
    }
  }
}

#(set-default-paper-size "a6" 'portrait)

\book {
  \score {
    \new Staff \relative c' {
      \repeat unfold 18 { a b c d \break }
    }
  }
}
