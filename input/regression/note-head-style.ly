\version "2.3.4"
\header{
texidoc="
Note head shapes may be set from several choices.  
The stem endings should be adjusted according to the note head.  
If you want different note head styles on one stem,
you must create a special context.

Harmonic notes have a different shape and different
dimensions. 
"
}

pattern =  <<
      \new Voice {
        \override Stem  #'direction = #UP
        e'4
         e'2. e'1 e'\breve*1/2 e'\longa*1/4
      }
      \new Voice {
        \override Stem  #'direction = #DOWN
         a4 a2. a1 a\breve*1/2 a\longa*1/4
      }
    >>

\score {
   \transpose c c {
    \clef C

    \override Staff.NoteHead  #'style = #'default
    s1*0^\markup { "default" }
    \pattern

    \override Staff.NoteHead  #'style = #'baroque
    s1*0^\markup { "baroque" }
    \pattern
        \break

    \override Staff.NoteHead  #'style = #'neo_mensural
    s1*0^\markup { "neomensural" }
    \pattern

    \override Staff.NoteHead  #'style = #'mensural
    s1*0^\markup { "mensural" }
    \pattern
    
    \break

    \override Staff.NoteHead  #'style = #'harmonic
    s1*0^\markup { "harmonic" }
    \pattern

    \override Staff.NoteHead  #'style = #'diamond
    s1*0^\markup { "diamond" }
    \pattern
    \break

    \override Staff.NoteHead  #'style = #'cross
    s1*0^\markup { "cross" }
    \pattern

    \override Staff.NoteHead  #'style = #'xcircle
    s1*0^\markup { "xcircle" }
\pattern
    
    \break

    \override Staff.NoteHead  #'style = #'triangle
    s1*0^\markup { "triangle" }
    \pattern
    

    \override Staff.NoteHead  #'style = #'slash
    s1*0^\markup { "slash" }
    \pattern
    \break
  }

  \paper {
    indent = 0.0
    raggedright = ##t
  }
}
