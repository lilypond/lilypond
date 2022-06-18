\version "2.23.10"

\header { texidoc = "New bar line glyphs can be defined in Scheme."
        }

\paper { ragged-right = ##t }


#(define (make-coda-sign-bar-line grob extent)
   (let ((stencil (ly:font-get-glyph (ly:grob-default-font grob)
                                     "scripts.coda")))
        stencil))

#(add-bar-glyph-print-procedure "0" make-coda-sign-bar-line)

\defineBarLine "0" #'(#t #f "")


\relative \new StaffGroup <<
  \new Staff {
    c'4 c \bar "0" c c \bar "0" \break
    c1
  }
  \new Staff {
    c4 c c c
    c1
  }
>>
