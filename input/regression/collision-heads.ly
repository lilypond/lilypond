\version "1.5.68"
\header {
    texidoc =

    "If @code{merge-differently-headed}, then
open note heads may be merged with black noteheads, but only
if the black note heads are from 8th or shorter notes.
"
    
}
\score { \notes \context Staff\relative c''<
\context Voice = VA {
    \voiceOne
    c2 c8 c4.
    
    \property Staff.NoteCollision \override #'merge-differently-headed = ##t
    c2
    c8 c4.
    c2
}
\context Voice = VB {
    \voiceTwo
    c8 c4.
    c2
    c8 c4.
    c2
    c4
}
    >
	 }
