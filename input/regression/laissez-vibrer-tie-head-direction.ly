\version "2.19.21"
\header {

  texidoc = "The 'head-direction of a LaissezVibrerTieColumn should
be able to be set without causing a segmentation fault."

}

\relative {
  c''2 \laissezVibrer
  \once \override LaissezVibrerTieColumn.head-direction = #RIGHT
  c \laissezVibrer
}

