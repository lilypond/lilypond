\version "2.3.4"
\header {
    texidoc =

    "If @code{merge-differently-headed} is enabled, then
open note heads may be merged with black noteheads, but only
if the black note heads are from 8th or shorter notes.
"
    
}

\paper { raggedright= ##t }


\score {
    \context Staff  \relative c'' <<
	{
	    c2 c8 c4.
	    
	    \override Staff.NoteCollision  #'merge-differently-headed = ##t
	    c2 c8 c4.
	    c2
	}\\
	{
	    c8 c4. c2
	    
	    c8 c4. c2
	    c4
	}
    >>
}
