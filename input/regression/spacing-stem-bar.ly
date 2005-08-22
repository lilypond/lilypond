
\version "2.6.0"
\header {

  texidoc = "Upstem notes before a barline are printed with some extra
space. This is an optical correction similar to juxtaposed stems.
"

}

\layout { raggedright = ##t}

\relative e'
{

				%%\override Staff.StaffSpacing  #'stem-spacing-correction = #0.5
				%%\override Staff.NoteSpacing  #'stem-spacing-correction = #0.5

  \time 3/8
  \stemUp
  e8 e e
  f f f
  a a a
  c c c
  e e e
}



