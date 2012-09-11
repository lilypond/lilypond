\version "2.16.0"

\header{
  texidoc= "This tests @code{\once} applied to multiple property operations."
}

\relative c' {
  c4 d \hideNotes e4 f |
  \unHideNotes g a \once \hideNotes b c |
}
