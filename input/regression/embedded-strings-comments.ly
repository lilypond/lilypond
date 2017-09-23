\version "2.21.0"

\header {
  texidoc = "Strings and comments inside of @code{#@{@dots{}#@}}
should not be confiusing to the embedded LilyPond parser.  If this
test succeeds, three notes with @code{(#)}, @code{($)}, and @code{(%)}
underneath will get displayed here."
}

\layout { ragged-right = ##t }

$#{ c'4-"(#)" %
    e'-"($)" %{$%}g'2-"(%)" %{%}
#}
