\version "2.27.6"

\header {
  texidoc = "Isolated percent signs in block comments in embedded LilyPond don't
confuse the parser.  No output."
}


##{ %{ test % test %} #}
