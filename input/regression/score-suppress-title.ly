\version "2.23.10"

\header {
  texidoc = "It works to set titling fields to @code{##f} on
score level while they have been defined to markup values
in the global header."
}

\header {
  piece = "piece"
  opus = "opus"
}

\score {
  \header {
    piece = ##f
    opus = ##f
  }
  { c' }
}
