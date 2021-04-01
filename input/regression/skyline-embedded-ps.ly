\header {

  texidoc = "If no outline is available (eg. for embedded PS),
  the bounding box is used as a fallback."
  
}

\version "2.22.0"

#(ly:set-option 'debug-skylines #t)

#(define-markup-command (emptybox layout props)
  ()
  (ly:make-stencil
   (list 'embedded-ps
         "")
   '(0 . 10) '(0 . 10)))

\score {
  <<
    \new Staff { c' }
    \new Staff { c'^\markup \emptybox }
  >>
}
