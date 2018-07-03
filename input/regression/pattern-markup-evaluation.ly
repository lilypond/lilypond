\version "2.18.0"

\header {
  texidoc = "@code{\\pattern} and @code{\\fill-with-pattern} markup
commands should interpret their arguments only once.  This test
calls them with a markup command that counts how often it is
evaluated.  The first line is supposed to show just @samp{1}
multiple times, the second line uses numbers @samp{2} to @samp{4}."
}

#(let ((n 0))
  (define-markup-command (another layout props) ()
   (set! n (1+ n))
   (interpret-markup layout props (number->string n))))

\markup \pattern #10 #X #2 \another
\markup \fill-with-pattern #2 #RIGHT \another \another \another
