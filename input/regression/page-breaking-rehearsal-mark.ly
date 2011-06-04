\version "2.14.0"

\header {
  texidoc = "The height of RehearsalMarks is taken into account during page
breaking."
}

#(set-default-paper-size "a6")

\book {
  \repeat unfold 2
  { \mark \markup \column {A B C D E F G H I J K L M N O P Q R S T U V W X Y Z} c1 \break }
}