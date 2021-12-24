\version "2.23.6"

\header {
  texidoc = "Callbacks returning @code{*unspecified*} are only
run once, as all callbacks."
}

{
  \override NoteHead.stencil =
    #(let ((run #f))
       (lambda (grob)
         (if run
             (ly:error "callback run twice"))
         (set! run #t)))
  c'
}
