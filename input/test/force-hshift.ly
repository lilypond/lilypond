
\version "2.3.4"
\header {
    texidoc="@cindex Force hshift
Horizontal shift (@code{hshift}) can be forced in order to avoid collisions. " }

\score { 
     \relative c'
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

