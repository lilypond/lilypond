\version "2.23.13"

\header {
  lsrtags = "expressive-marks, tweaks-and-overrides, version-specific"

  texidoc = "
By default, LilyPond does not allow the same articulation (e.g., an
accent, a fermata, a flageolet, etc.) to be displayed above and below a
note. For example, @code{c4_\\fermata^\\fermata} only shows a fermata
below. The fermata above gets simply ignored.

However, one can stick scripts (just like fingerings) inside a chord,
which means it is possible to have as many articulations as desired.
This approach has the advantage that it ignores the stem and positions
the articulation relative to the note head. This can be seen in the
case of the flageolets in the snippet. To mimic the behaviour of
scripts outside a chord, @code{'add-stem-support} would be required.

The solution is thus to write the note as a chord and add the
articulations inside of @code{<...>}, using the direction modifiers
@code{^} and @code{_} as appropriate.
"

  doctitle = "Showing the same articulation above and below a note or chord"
}


\relative c' {
  <>^"Wrong"
  c2_\fermata^\fermata % The second fermata is ignored!
  <e d'>2^\flageolet_\flageolet

  \stopStaff s1 \startStaff

  <>^"Works if written inside a chord"
  <e_\flageolet d'^\flageolet>2
  <e_\flageolet d'^\flageolet>2
  <e_\flageolet^\flageolet>2
  <e_\fermata^\fermata>2
}
