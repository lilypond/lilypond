
\header {
texidoc = "Space is allowed for the actual size of accidentals on tied notes."
}

\version "2.19.21"
\paper {ragged-right = ##t }
\relative {
  \clef treble
  \time 3/4
  c'8 b2  <g b des f>8 ~ |
  <g b des f>8 r
  % Large accidental
  \override Staff.Accidental.stencil =
  #(lambda (g)
     (let ((alt (ly:grob-property g 'alteration)))
       (grob-interpret-markup g
         (make-circle-markup (number->string alt)))))
  bes4 ~ bes ~ | bes ~ bes bes ~ | \break
  bes ~ bes bes
}

