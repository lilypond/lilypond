\version "2.13.8"

\header{ texidoc = "As default, tablature staves show only the fret numbers, because
                    in most situations, they are combined with normal staves.
                    When used without standard notation, @code{tabFullNotation}
                    can be used."
       }

tabstuff = {
  \time 3/4
  \compressFullBarRests
  c4^"test" d( e)
  f4\f g a^\fermata
  R2.*3
  c8\<\( c16 c ~ c2\!
  c'2.\)
  \mark \default
  R2.
  \ottava #1
  r4 d'4 r8 e
  \ottava #0
  \times 3/4 { b,4 c \glissando d\5 \glissando c }
  c4. d-_( e\varcoda)
  ->f g~ a\prall g\thumb e-.
  \bar "|."
}

\score {
  <<
    \new Staff { \clef "G_8" \tabstuff }
    \new TabStaff   { \tabstuff }
  >>
}

\score {
  \new TabStaff {
    \tabFullNotation
    \tabstuff
  }
}
