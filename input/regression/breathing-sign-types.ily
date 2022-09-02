\version "2.23.9"

test =
#(define-music-function (breathtype) (symbol?)
  #{
    \fixed c' {
      b2-\markup \tiny { * #(symbol->string breathtype) }
      \set Voice.breathMarkType = #breathtype
      \breathe
      b2
      \breathe
    }
  #} )

music = \fixed c' {
  b2-\markup \tiny "(default)" \breathe b2 \breathe
  \test #'caesura
  \test #'chantdoublebar
  \test #'chantfullbar
  \test #'chanthalfbar
  \test #'chantquarterbar
  \test #'comma
  \test #'curvedcaesura
  \test #'outsidecomma
  \test #'spacer
  \test #'tickmark
  \test #'upbow
  \test #'varcomma
}
