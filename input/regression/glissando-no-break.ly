\version "2.19.21"
#(ly:set-option 'warning-as-error #t)
#(ly:expect-warning (_ "forced break was overridden by some other event, should you be using bar checks?"))

\header {

  texidoc = "Glissandi are not broken. Here a @code{\\break} is ineffective.
Use @code{breakable} grob property to override."

}

\layout {
  ragged-right = ##t
}
\relative {
  c'1 
  c1\glissando
  \break
  d'1
  d1
} 
