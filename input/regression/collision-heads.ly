#(ly:set-option 'old-relative)
\version "2.1.7"
\header {
    texidoc =

    "If @code{merge-differently-headed}, then
open note heads may be merged with black noteheads, but only
if the black note heads are from 8th or shorter notes.
"
    
}
    \paper { raggedright= ##t }


\score { \notes \context Staff\relative c''<<
\new Voice {
    \voiceOne
    c2 c8 c4.
    
    \property Staff.NoteCollision \override #'merge-differently-headed = ##t
    c2
    c8 c4.
    c2
}
\new Voice {
    \voiceTwo
    c8 c4.
    c2
    c8 c4.
    c2
    c4
}
    >>
	 }
