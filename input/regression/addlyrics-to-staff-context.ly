\version "2.19.14"

\header {
  texidoc = "
@code{\\addlyrics} may get used on a @code{Staff} context and will
then consider all note events created below it for synchronization.
"
}

\relative \new Staff {
  \time 2/4
  c''4 b8. a16 g4.
  << { r8 a4( b) c2 } \\
     { f,8 e4 d c2 }
  >>
}
\addlyrics {
  Life is __ _ love, _ live __ _ life.
}
\addlyrics {
  No more let sins and sor -- rows grow.
}
