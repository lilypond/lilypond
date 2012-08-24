\version "2.16.0"
#(ly:set-option 'warning-as-error #f)
#(ly:expect-warning (_ "forced break was overridden by some other event, should you be using bar checks?"))

\header {

  texidoc = "Glissandi are not broken. Here a @code{\\break} is ineffective.
Use @code{breakable} grob property to override."

}

\layout {
  ragged-right = ##t
}
\relative c' {
  c1 
  c1\glissando
  \break
  d'1
  d1
} 
