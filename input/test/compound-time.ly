
\version "2.10.0"
\header {
texidoc = "@cindex compound time
@cindex plus

Compound time signatures can be printed.  Automatic beaming works in
compound time.

"
}

\layout{ragged-right = ##t}

#(define (compound-time grob one two num)
  (interpret-markup
   (ly:grob-layout grob)
   '(((baseline-skip . 2)
      (word-space . 2)
      (font-family . number)))
   (markup
    #:line ( #:column (one num) #:lower 1 "+" #:column (two num)))))

\relative {
  %% compound time signature hack
  \time 5/8
  \override Staff.TimeSignature  #'stencil
  = #(lambda (grob) (compound-time grob "2" "3" "8"))
  #(override-auto-beam-setting '(end 1 8 5 8) 1 4)
  c8 c c8 c c
}

