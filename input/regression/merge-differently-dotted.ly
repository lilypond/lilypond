\header {

texidoc = "If NoteCollision has merge-differently-dotted set, note
heads that have differing dot counts may be merged anyway."

 }
	
\score { \notes {
  \context Staff <
  \context Voice = VA { \voiceOne g'8 g'8 
     \property Staff.NoteCollision \override #'merge-differently-dotted = ##t
     g'8 g'8
     }
  \context Voice = VB { \voiceTwo [g'8. f16] [g'8. f'16] } 
  >
}}
