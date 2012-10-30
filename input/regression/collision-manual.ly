\header {

  texidoc = "Collision resolution may be forced manually with @code{force-hshift}. "
}
\version "2.17.6"

\paper  {
  ragged-right = ##t
}

\relative c' {
  << {  f
	\override NoteColumn.force-hshift = #0.1
	f } \\
     {  e  e }
   >> 
}
