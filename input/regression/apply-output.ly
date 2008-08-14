\version "2.11.51"

\header {
  texidoc = "The @code{\applyOutput} expression is the most flexible way to
tune properties for individual grobs.

Here, the layout of a note head is changed depending on its vertical
position.
"
}

\layout {
  ragged-right = ##t
}

#(define (mc-squared gr org cur)
  (let*
   (
     (ifs (ly:grob-interfaces gr))
     (sp (ly:grob-property gr 'staff-position))
   )
   (if (memq 'note-head-interface ifs)
    (begin
     (ly:grob-set-property! gr 'stencil ly:text-interface::print)
     (ly:grob-set-property! gr 'font-family 'roman)
     (ly:grob-set-property! gr 'text
      (make-raise-markup -0.5
       (case sp
	((-5) (make-simple-markup "m"))
	((-3) (make-simple-markup "c "))
	((-2) (make-smaller-markup (make-bold-markup "2")))
	(else (make-simple-markup "bla"))
      ))))
  )))

\context Voice \relative c' {
  \stemUp
  \set autoBeaming = ##f

  { <d f g b>8

    \applyOutput #'Voice #mc-squared
    <d f g b>
  }
}

% EOF
