\version "2.25.33"

\header {
    texidoc = "Simple-fraction components of a complex time signature are
numeric regardless of the time signature style.
"
}

\relative {
  \timeAbbrev #'(2 2) b'1
  \timeAbbrev #'(4 4) b1
  \timeAbbrev #'((1 1) (2 2)) b\breve
  \timeAbbrev #'((2 2) (4 4)) b\breve
  \timeAbbrev #'((4 4) (3 3 2 8)) b\breve
}
