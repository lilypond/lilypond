\version "2.13.27"
\header {

  texidoc = "The 'head-direction of a LaissezVibrerTieColumn should
be able to be set without causing a segmentation fault."

}

\relative c'' {
  c2 \laissezVibrer
  \once \override LaissezVibrerTieColumn #'head-direction = #RIGHT
  c \laissezVibrer
}

