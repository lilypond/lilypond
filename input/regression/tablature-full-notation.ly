\version "2.13.4"

\header{ texidoc = "As default, tablature staves show only the fret numbers, because
                    in most situations, they are combined with normal staves.
                    When used without standard notation, @code{tabFullNotation}
                    can be used."
       }

tabstuff = {
  \time 3/4
  c4^"test" d( e)
  f4\f g a^\fermata
  c8\<\( c16 c ~ c2\!
  c'2.\)
  \mark \default
  R2.
  r4 d4 r8 e
  \times 3/4 { b4 c \glissando d\5 \glissando c }
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
