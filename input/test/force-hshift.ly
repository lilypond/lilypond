
\version "2.1.7"
\header {
    texidoc="@cindex Force hshift
Force hshift to override collisions. " }

\score { 
     \notes\relative c'
       \context Staff << {
			<d g>
			<d g>
		} \\ {
			<b f'>
\once \property Voice.NoteColumn
  \override #'force-hshift = #1.7
			<b f'>
		}
	>> 
  \paper {
    raggedright = ##t
  }  
}

