\version "2.12.0"

\header {
  texidoc = "Melismata may be entered manually by substituting
@code{_} for lyrics on notes that are part of the melisma."
}

\relative c' {
  \set melismaBusyProperties = #'()
  c4 d( e) f
  f4( e) e e
}
\addlyrics {
  Ky -- _ _ ri __ _ _ _  e
}
