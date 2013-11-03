\version "2.17.30"

\header {

  texidoc = "A grace note after @code{\\cadenzaOff} does not keep
  autobeaming from resuming properly."

}

\layout { ragged-right = ##t }

\relative c' {
  e8 e e e e e e e
  \cadenzaOn <>^\markup \typewriter "\\cadenzaOn"
  e e e e e e e e
  \cadenzaOff
  \bar "|"
  <>^\markup \typewriter "\\cadenzaOff"
  \grace f8
  e e e e e e e e
}
