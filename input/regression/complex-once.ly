\version "2.15.17"

\header{
  texidoc= "This tests @code{\once} applied to multiple property operations."
}

\relative c' {
  c4 d \hideNotes e4 f |
  \unHideNotes g a \once \hideNotes b c |
}
