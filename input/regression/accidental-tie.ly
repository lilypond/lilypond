\version "2.1.25"

\header {

    texidoc = "The second and third notes should not get accidentals,
    because they are tied to a note.  However, an accidental is
    present if the line is broken at the tie, which happens for the G
    sharp."

}

mus = \notes 	\relative c' {
    f1~
    f2~f4 % ~ f8
    fis8  gis8 ~
    \break
    gis1
}

\score {
    \notes <<
	\new NoteNames \mus
	\new Voice { \key g \major \mus }
    >>
    \paper {
	raggedright = ##t
    }
}
