\version "2.10.12"

\header { texidoc = "
The layout of the major 7 can be tuned with @{majorSevenSymbol@}
" }

\chords {
  c:7+
  \set majorSevenSymbol = \markup { "j7" }
  c:7+
}
