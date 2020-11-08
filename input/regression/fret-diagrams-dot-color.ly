\version "2.23.0"

\header {
  texidoc = "Dots in fret diagrams may be colored as well as the entire fret
diagram.
Not explicitely colored dots take the color from @code{TextScript} grob or from
@code{with-color}.  Otherwise the specified color is preserved.
Parentheses take their color from the dot, if @code{default-paren-color} is used
they take their color from the overall color.
Works for inverted dots as well."
}

defaultMrkp =
  \markup
    \fret-diagram-verbose
      #'((place-fret 1 1 1))

blueDotMrkp =
  \markup
    \fret-diagram-verbose
      #'((place-fret 1 1 1 blue))

parenthesizedBlueDotMrkpI =
  \markup
    \fret-diagram-verbose
      #'((place-fret 1 1 1 blue parenthesized))

parenthesizedBlueDotMrkpII =
  \markup
    \fret-diagram-verbose
      #'((place-fret
          1 1 1
          blue
          parenthesized
          default-paren-color))

invertedDefaultMrkp =
  \markup
    \fret-diagram-verbose
      #'((place-fret 1 1 1 inverted))

invertedBlueDotMrkp =
  \markup
    \fret-diagram-verbose
      #'((place-fret 1 1 1 inverted blue))

invertedParenthesizedBlueDotMrkpI =
  \markup
    \fret-diagram-verbose
      #'((place-fret 1 1 1 inverted blue parenthesized))

invertedParenthesizedBlueDotMrkpII =
  \markup
    \fret-diagram-verbose
      #'((place-fret
          1 1 1
          inverted
          blue
          parenthesized
          default-paren-color))

{
  \textLengthOn
  \override TextScript.self-alignment-X = #CENTER
  \override TextScript.font-size = #-4
  \override TextScript.baseline-skip = 1.5

  f''^\defaultMrkp
     ^\markup \column { "black dot" "black rest" }
  f''^\blueDotMrkp
     ^\markup \column { "blue dot" "black rest" }
  f''^\parenthesizedBlueDotMrkpI
     ^\markup \column { "blue dot" "blue parens" }
  f''^\parenthesizedBlueDotMrkpII
     ^\markup \column { "blue dot" "black parens" }

  f''^\invertedDefaultMrkp
     ^\markup \column { "black dot" "black rest" }
  f''^\invertedBlueDotMrkp
     ^\markup \column { "blue dot" "black rest" }
  f''^\invertedParenthesizedBlueDotMrkpI
     ^\markup \column { "blue dot" "blue parens" }
  f''^\invertedParenthesizedBlueDotMrkpII
     ^\markup \column { "blue dot" "black parens" }

  \override TextScript.color = #red

  f''^\defaultMrkp
     ^\markup \column { "red dot" "red rest" }
  f''^\blueDotMrkp
     ^\markup \column { "blue dot" "red rest" }
  f''^\parenthesizedBlueDotMrkpI
     ^\markup \column { "blue dot" "blue parens" }
  f''^\parenthesizedBlueDotMrkpII
     ^\markup \column { "blue dot" "red parens" }

  f''^\invertedDefaultMrkp
     ^\markup \column { "red dot" "red rest" }
  f''^\invertedBlueDotMrkp
     ^\markup \column { "blue dot" "red rest" }
  f''^\invertedParenthesizedBlueDotMrkpI
     ^\markup \column { "blue dot" "blue parens" }
  f''^\invertedParenthesizedBlueDotMrkpII
     ^\markup \column { "blue dot" "red parens" }
}
