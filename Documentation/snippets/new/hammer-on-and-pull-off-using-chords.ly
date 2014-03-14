\version "2.16.0"

\header {
  lsrtags = "editorial-annotations, fretted-strings"

  texidoc = "
When using hammer-on or pull-off with chorded notes, only a single arc
is drawn. However @q{double arcs} are possible by setting the
@code{doubleSlurs} property to @code{#t}.

"
  doctitle = "Hammer on and pull off using chords"
}

\new TabStaff {
  \relative c' {
    % chord hammer-on and pull-off
    \set doubleSlurs = ##t
    <g' b>8( <a c> <g b>)
  }
}
