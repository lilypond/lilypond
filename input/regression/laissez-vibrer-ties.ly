
\header {


  texidoc = "
l.v. ties should avoid dots and staff lines, similar to normal ties.
They have fixed size. Their formatting can be tuned with
@code{tie-configuration}.

"
  }

\version "2.19.21"
\paper {
  ragged-right = ##t
}

\relative {
  <c' e g>\laissezVibrer r4
  <c f g>\laissezVibrer r
  <c d f g>\laissezVibrer r
  <c d f g>4.\laissezVibrer r8

  <c d e f>4\laissezVibrer r
  \override LaissezVibrerTieColumn.tie-configuration
  = #'((-7 . -1)
       (-5 . -1)
       (-3 . 1)
       (-1 . 1))
  <c d e f>4\laissezVibrer r
}
