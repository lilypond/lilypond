\version "2.16.0"

\header {
  texidoc = "
@code{\laissezVibrer} ties should also work on individual notes of a chord.
" }

\relative c' {
  <d-\laissezVibrer g>1
  <d^\laissezVibrer g_\laissezVibrer>1
}
