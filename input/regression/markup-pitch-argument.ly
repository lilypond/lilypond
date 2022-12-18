\version "2.25.0"

\header {
  texidoc = "A markup command can take a pitch argument
and receive it between braces."
}

#(define-markup-command (take-pitch layout props arg) (ly:pitch?)
   (interpret-markup layout props #{ \markup \score { $arg 4 } #}))

\markup \take-pitch { bes }
