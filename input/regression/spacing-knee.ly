\version "2.17.6"
\header {

  texidoc = "For knees, the spacing correction is such that the
stems are put at regular distances. This effect takes into account the
width of the note heads and the thickness of the stem.
"
}

\layout { ragged-right = ##t}

{
  g''8[ g g'' g''] 

				% check code independent of default settings.
  \override NoteSpacing.knee-spacing-correction = #1.0 
  g''8[ g g'' g''] 
  \override Stem.thickness = #10 
  g''8[ g g'' g''] 
}




