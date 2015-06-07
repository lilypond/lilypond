\version "2.19.21"

\header {
  texidoc = "The default behavior of tuplet-bracket visibility is to print a bracket
unless there is a beam of the same length as the tuplet.  Overriding
@code{'bracket-visibility} changes the bracket visibility as follows:

@itemize

@item
@code{#t} (always print a bracket)

@item
@code{#f} (never print a bracket)

@item
@code{'if-no-beam} (only print a bracket if there is no beam)

@end itemize
"
}

music = \relative {
  \tuplet 3/2 { c''16[ d e } f8]
  \tuplet 3/2 { c8 d e }
  \tuplet 3/2 { c4 d e }
}

\new Voice {
  \relative c' {
    << \music s4^"default" >>
    \override TupletBracket.bracket-visibility = #'if-no-beam
    << \music s4^"'if-no-beam" >>
    \override TupletBracket.bracket-visibility = ##t
    << \music s4^"#t" >>
    \override TupletBracket.bracket-visibility = ##f
    << \music s4^"#f" >>
  }
}
