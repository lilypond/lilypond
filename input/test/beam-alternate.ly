
\header {

    texidoc = "Meshing eighths beams, connected by ties. This occurs
    in (for example) in the cello suites. See also
    @file{tie-cross-voice.ly}."

}

\version "2.1.26"

wipeNote = {
    \once \override NoteHead #'transparent = ##t
    \once \override Stem #'transparent = ##t 
}


\score {
    \notes \relative c'' {
	<< {
	    c8[~
	    \wipeNote
	    c8
	    c8~
	    \wipeNote
	    c~
	    c]
	}\\
	   { s8 c8 [ s c s c] }

	   
       >>
    }
    \paper { raggedright = ##t }
}
