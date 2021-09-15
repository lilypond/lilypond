\version "2.23.4"

\header {
  texidoc = "A warning is emitted when a markup command does not return
a stencil as it should."
}

#(ly:set-option 'warning-as-error #t)
#(for-each
   (lambda (i)
     (ly:expect-warning "markup interpretation must yield stencil"))
   (iota 2))

#(define-markup-command (foo layout props) ()
   42)

\markup \foo

{ c'^\markup \foo }
