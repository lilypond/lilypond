\version "2.23.2"

\header {
  lsrtags = "percussion, preparing-parts, really-simple, rhythms, specific-notation"

  texidoc = "
A tam-tam example, entered with 'tt'
"

  doctitle = "Tam-tam example"
}


#(define mydrums '((tamtam default #f 0)))

\new DrumStaff \with { instrumentName = #"Tamtam" }

\drummode {
  \set DrumStaff.drumStyleTable = #(alist->hash-table mydrums)
  \override Staff.StaffSymbol.line-positions = #'( 0 )
  \override Staff.BarLine.bar-extent = #'(-1.5 . 1.5)

  tt 1 \pp \laissezVibrer
}
