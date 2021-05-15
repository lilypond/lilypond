\version "2.23.3"

\header {
  texidoc = "A warning is emitted when a top-level markup command does
not return a stencil as it should."
}

#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning "markup interpretation must yield stencil")

#(define-markup-command (foo layout props) ()
   42)

\markup \foo
