\version "2.19.21"

\header{
  texidoc= "This tests @code{\\once} applied to multiple property operations."
}

\relative {
  c'4 d \hideNotes e4 f |
  \unHideNotes g a \once \hideNotes b c |
}
