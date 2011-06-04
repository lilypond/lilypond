\version "2.14.0"

\header {
  texidoc = "
The @code{\\path} markup command supports the
@code{line-join-style} property with values of @code{bevel},
@code{round}, and @code{miter}.
"
}

myPath =
#'((moveto 0 0) (rlineto 2 5) (rlineto 2 -5))

\markup {
  \column {
    \override #'(line-join-style . bevel) {
      \path #1 #myPath
    }
    \override #'(line-join-style . round) {
      \path #1 #myPath
    }
    \override #'(line-join-style . miter) {
      \path #1 #myPath
    }
  }
}
