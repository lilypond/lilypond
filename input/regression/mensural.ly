\version "1.3.146"
\header {
    
texidoc = "There is limited support for mensural notation: note head
shapes are available. Mensural stems are centered on the note heads,
both for up and down stems."

}


\score {\notes { \context Voice { 
    \property Voice.NoteHead \set #'font-family = #'ancient
    \property Voice.NoteHead \override #'style = #'mensural
\transpose d''' {  c4 c2 c8  c16 c16  c1 c\breve c\longa }
\transpose c'' { c4 c2 c8  c16 c16  c1 c\breve c\longa }
}}}
