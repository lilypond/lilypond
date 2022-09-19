\version "2.23.14"

\header {
  texidoc = "The height of marks is taken into account during page
breaking."
}

#(set-default-paper-size "a6")

\book {
  \repeat unfold 2
  { \textMark \markup \column {A B C D E F G H I J K L M N O P Q R S T U V W X Y Z} c1 \break }
}
