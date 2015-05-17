\version "2.19.21"

\header {

  texidoc = "A grace note after @code{\\cadenzaOff} does not keep
  autobeaming from resuming properly."

}

\layout { ragged-right = ##t }

\relative {
  e'8 e e e e e e e
  \cadenzaOn <>^\markup \typewriter "\\cadenzaOn"
  e e e e e e e e
  \cadenzaOff
  \bar "|"
  <>^\markup \typewriter "\\cadenzaOff"
  \grace f8
  e e e e e e e e
}
