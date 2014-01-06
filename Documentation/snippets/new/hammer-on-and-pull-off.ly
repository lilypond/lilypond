\version "2.16.0"

\header {
  lsrtags = "editorial-annotations, fretted-strings"

  texidoc = "
Hammer-on and pull-off can be obtained using slurs.

"
  doctitle = "Hammer on and pull off"
}

\new TabStaff {
  \relative c' {
    d4( e\2)
    a( g)
  }
}
