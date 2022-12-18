\version "2.25.0"

\header {
  texidoc = "Markup commands can take music arguments,
enclosed in braces.  A bare pitch or duration is accepted."
}

#(define-markup-command (take-music layout props arg) (ly:music?)
   (interpret-markup layout props #{ \markup \score { #arg } #}))

\markup \take-music { c'4 c'2 c'4 }
\markup \take-music { 8 }
\markup \take-music { b }
