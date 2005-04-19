
\version "2.5.20"
\header {
texidoc = "@cindex compound time
@cindex plus

Compound time signatures can be printed.

"
}

\layout{raggedright = ##t}

#(define (compound-time grob one two num)
  (interpret-markup
   (ly:grob-layout grob)
   '(((baseline-skip . 2)
      (word-space . 2)
      (font-family . number)))
   (markup
    #:line ( #:column (one num) #:lower 1 "+" #:column (two num)))))
%    ;;  #:line ( #:column (one num) #:lower 1 "+" #:column (two num)))))

\relative {
  %% compound time signature hack
  \time 5/8
  \override Staff.TimeSignature #'print-function
  = #(lambda (grob) (compound-time grob "2" "3" "8"))

  %% manual beaming, auto beam engraver cannot handle compound time,
  %% it extends 2/8 pattern to 4/8, which does not work.

  %% Hmm, why don't we just junk the modulo functionality, and
  %% write-out all endings explicitely, we get compound time handling
  %% for free?
  
  c4 c8[ c c]
}
