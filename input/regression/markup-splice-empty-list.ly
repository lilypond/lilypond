\version "2.23.10"

\header {
  texidoc = "It works to splice an empty list inside markup."
}

\markup \with-color #red { a #@'() b }
\markup \with-color #red { #@'() }
\markuplist { a #@'() b }
\markuplist { #@'() }
