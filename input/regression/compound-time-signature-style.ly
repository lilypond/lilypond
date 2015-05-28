\version "2.19.21"

\header {
    texidoc = "Simple-fraction components of a compound time signature are numeric regardless of the time signature style.
"
}

\relative {
  \compoundMeter #'(2 2) b'1
  \compoundMeter #'(4 4) b1
  \compoundMeter #'((1 1) (2 2)) b\breve
  \compoundMeter #'((2 2) (4 4)) b\breve
  \compoundMeter #'((4 4) (3 3 2 8)) b\breve
}
