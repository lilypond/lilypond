\version "2.19.21"

\header {
  texidoc = "Melismata may be entered manually by substituting
@code{_} for lyrics on notes that are part of the melisma."
}

\relative {
  \set melismaBusyProperties = #'()
  c'4 d( e) f
  f4( e) e e
}
\addlyrics {
  Ky -- _ _ ri __ _ _ _  e
}
