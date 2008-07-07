\header {

  texidoc = "Glissandi are not broken. Here a @code{\break} is ineffective.
Use @code{breakable} grob property to override."

}
\version "2.11.51"
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
