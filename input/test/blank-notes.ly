\version "1.5.68"

\header {
    
    texidoc = " print lesson sheets that contain blank lines and just portions of blank lines."

}

blanknotes = { \property Voice.NoteHead
	       \override #'transparent  = ##t
	       \property Voice.Stem
	       \override #'transparent = ##t }
unblanknotes = { \property Voice.NoteHead
		 \revert #'molecule-callback
		 \property Voice.Stem
		 \revert #'molecule-callback }


\score {
    \notes { c4 d4 
    \blanknotes e4 f4   \unblanknotes
	     g4 a 
	     }}

