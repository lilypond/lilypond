\version "2.16.0"
#(set-default-paper-size "a6")

\header {
  texidoc = "Minimal page breaker: special case when the last system is moved
to an other page when there is not enough space because of the tagline."
}

textBox = \markup \fill-line { \override #'(box-padding . 13) \box Text }

\book {
  \header {
    tagline = \markup \fill-line { \override #'(box-padding . 5) \box Tagline }
  }
  \paper {
    #(define page-breaking ly:minimal-breaking) 
  }

  \markup \textBox
  \markup \textBox
  \markup \textBox
  \markup \textBox
}
