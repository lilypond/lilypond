\version "2.16.0"

\header {
  texidoc = "
The @code{\\path} markup command allows the user to draw
arbitrary paths using a simple syntax.  The two paths below
should be identical.
"
}

\markup {
  \column {
    \path #0.2 #'((moveto 1 1)
		  (lineto 1 6)
		  (curveto 3 8 5 6 1 1)
		  (closepath))

    \path #0.2 #'((rmoveto 1 1)
		  (rlineto 0 5)
		  (rcurveto 2 2 4 0 0 -5)
		  (closepath))
  }
}
