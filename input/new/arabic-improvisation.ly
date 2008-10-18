\version "2.11.61"

\header {
  lsrtags = "world-music"
  texidoc = "For improvisations or @emph{taqasim} which are
temporarily free, the time signature can be omitted and
@code{\cadenzaOn} can be used.  Adjusting the accidental style
might be required, since the absence of bar lines will cause the
accidental to be marked only once.  Here is an example of what
could be the start of a @emph{hijaz} improvisation:"
doctitle = "Arabic improvisation"
}

\include "arabic.ly"

\relative sol' {
  \key re \kurd
  #(set-accidental-style 'forget)
  \cadenzaOn
  sol4 sol sol sol fad mib sol1 fad8 mib re4. r8 mib1 fad sol
}
