
\version "2.1.26"
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
\once \override NoteColumn  #'force-hshift = #1.7
			<b f'>
		}
	>> 
  \paper {
    raggedright = ##t
  }  
}

