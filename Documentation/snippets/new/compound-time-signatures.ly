\version "2.13.4"

\header {
  lsrtags = "rhythms"
  texidoc = "
Odd 20th century time signatures (such as \"5/8\") can often be played
as compound time signatures (e.g. \"3/8 + 2/8\"), which combine two or
more inequal metrics. LilyPond can make such music quite easy to read
and play, by explicitly printing the compound time signatures and
adapting the automatic beaming behavior. (Graphic measure grouping
indications can also be added; see the appropriate snippet in this
database.)
"
  doctitle = "Compound time signatures"
}

#(define ((compound-time one two num) grob)
  (grob-interpret-markup grob
    (markup #:override '(baseline-skip . 0) #:number
      (#:line (
          (#:column (one num))
          #:vcenter "+"
          (#:column (two num)))))))

\relative c' {
  \override Staff.TimeSignature #'stencil = #(compound-time "2" "3" "8")
  \time 5/8
  \overrideBeamSettings #'Staff #'(5 . 8) #'end
    #'((* . (2 3)))
  c8 d e fis gis
  c8 fis, gis e d
  c8 d e4 gis8
}
