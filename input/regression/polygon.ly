\version "2.23.3"

\header {
  texidoc = "The @code{\\polygon} markup command draws polygons
according to the properties @code{filled}, @code{thickness}
and @code{extroversion}."
}

regularPentagon =
#'((1 . 0) (0.31 . 0.95) (-0.81 . 0.59) (-0.81 . -0.59) (0.31 . -0.95))

\markup \scale #'(5 . 5) {
  \polygon #'((-1 . -1) (0 . -3) (2 . 2) (1 . 2))
  \override #'(filled . #f)
    \override #'(thickness . 2)
      \combine
        \with-color "blue"
          \polygon #regularPentagon
        \with-color "red"
          \override #'(extroversion . 1)
            \polygon #regularPentagon
}
