\version "2.14.0"

\header {
  texidoc = "Text that can spread over pages is entered with the
@code{\\markuplines} command. It can be assigned to a variable and inserted
at top-level with or without preceding it by @code{\\markuplines}."
}

#(set-default-paper-size "a6")

mytext = \markuplines {
  \justified-lines {
    Lorem ipsum dolor sit amet, consectetur adipisici elit, sed
    eiusmod tempor incidunt ut labore et dolore magna aliqua. ...
  }
}

\markuplines \mytext
\mytext
