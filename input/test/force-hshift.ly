
\version "1.9.1"
\header {
    texidoc="@cindex Force hshift
Force hshift to override collisions. " }

\score { 
     \notes\relative c'
       \context Staff < {
			\property Voice.NoteColumn \override #'force-hshift = #0.1
			\stemUp
	       		\property Voice.NoteColumn \override #'horizontal-shift = #1
			<<d a'>>
		} \\
		 {
			\stemDown
	       		\property Voice.NoteColumn \override #'horizontal-shift = #1
			\property Voice.NoteColumn \override #'force-hshift = #-0.1
			<<b f'>>
		}
	> 
  \paper {
    raggedright = ##t
  }  
}

