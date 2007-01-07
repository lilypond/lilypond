
\version "2.10.0"
\header {
texidoc = "@cindex compound time
@cindex plus

Compound time signatures can be printed.  Automatic beaming works in
compound time.

"
}

\layout{ragged-right = ##t}

#(define (compound-time one two num)
  (markup #:override '(baseline-skip . 0) #:number 
   (#:line ((#:column (one num)) #:vcenter "+" (#:column (two num))))))


\relative {
  %% compound time signature hack
  \time 5/8
  \override Staff.TimeSignature #'stencil = #ly:text-interface::print
  \override Staff.TimeSignature #'text = #(compound-time "2" "3" "8" )
  #(override-auto-beam-setting '(end 1 8 5 8) 1 4)
  c8 c c8 c c
}

