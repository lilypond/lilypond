\version "2.1.7"
\header {
    
    texidoc = "If NoteCollision has merge-differently-dotted = \\#t note
heads that have differing dot counts may be merged anyway.  Dots
should not disappear when merging similar note heads."
    
}

\paper { raggedright= ##t }
	
\score {
    \context Staff \notes\relative c'' <<
	{
	    g8[ g8]
	    \property Staff.NoteCollision
	    \override #'merge-differently-dotted = ##t
	    g8[ g8]
	    g4. r8 g8. g16
	    g8 g4 r8 g4
	}
	\\
	{
	    g8.[ f16]
	    g8.[ f16]
	    g8 g4 r8 g4
	    g4. r8 g8. g16
	}
    >>
}

