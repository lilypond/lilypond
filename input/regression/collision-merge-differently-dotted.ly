\version "2.1.7"
\header {

texidoc = "If NoteCollision has merge-differently-dotted set, note
heads that have differing dot counts may be merged anyway."

 }
    \paper { raggedright= ##t }


	
\score { \notes {
  \context Staff <<
  \new Voice { \voiceOne g'8 g'8 
     \property Staff.NoteCollision \override #'merge-differently-dotted = ##t
     g'8 g'8
     }
  \new Voice { \voiceTwo  g'8.[ f16]  g'8.[ f'16] } 
  >>
}}

