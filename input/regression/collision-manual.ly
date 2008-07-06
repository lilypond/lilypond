\header {

  texidoc = "Collision resolution may be forced manually with @code{force-hshift}. "
}
\version "2.11.51"

\paper  {
  ragged-right = ##t
}

\relative {
  << {  f
	\override NoteColumn #'force-hshift = #0.1
	f } \\
     {  e  e }
   >> 
}
