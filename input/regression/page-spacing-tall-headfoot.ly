\version "2.16.0"

#(set-default-paper-size "a6")

\book {
  \header {
    texidoc = "Both the page breaking and the page layout take account of
the heights of the header and footer."
  }
  \paper {
    oddHeaderMarkup = \markup { \fill-line { \column { t a l l h e a d e r } } } 
    oddFooterMarkup = \markup { \fill-line { \column { t a l l f o o t e r } } }
    evenFooterMarkup = \markup { \fill-line { "small footer" } }
  }

  \repeat unfold 10 { c'1 \break }
}


