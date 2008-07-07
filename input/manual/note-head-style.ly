\version "2.11.51"
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

\layout {
  indent = 0.0
  ragged-right = ##t
}

pattern = <<
  \new Voice {
    \override Stem  #'direction = #UP
    e'4 e'2. e'1 e'\breve*1/2 e'\longa*1/4
  }
  \new Voice {
    \override Stem  #'direction = #DOWN
    a4 a2. a1 a\breve*1/2 a\longa*1/4
  }
>>


\transpose c c {
  \clef C

  \override Staff.NoteHead  #'style = #'default
  s1*0^\markup { "default" }
  \pattern

  \override Staff.NoteHead  #'style = #'baroque
  s1*0^\markup { "baroque" }
  \pattern

  \break

  \override Staff.NoteHead  #'style = #'neomensural
  s1*0^\markup { "neomensural" }
  \pattern

  \override Staff.NoteHead  #'style = #'mensural
  s1*0^\markup { "mensural" }
  \pattern

  \break

  \override Staff.NoteHead  #'style = #'petrucci
  s1*0^\markup { "petrucci" }
  \pattern

  \override Staff.NoteHead  #'style = #'harmonic
  s1*0^\markup { "harmonic" }
  \pattern

  \break

  \override Staff.NoteHead  #'style = #'harmonic-black
  s1*0^\markup { "harmonic-black" }
  \pattern

  \override Staff.NoteHead  #'style = #'harmonic-mixed
  s1*0^\markup { "harmonic-mixed" }
  \pattern

  \break

  \override Staff.NoteHead  #'style = #'diamond
  s1*0^\markup { "diamond" }
  \pattern

  \override Staff.NoteHead  #'style = #'cross
  s1*0^\markup { "cross" }
  \pattern

  \break

  \override Staff.NoteHead  #'style = #'xcircle
  s1*0^\markup { "xcircle" }
  \pattern

  \override Staff.NoteHead  #'style = #'triangle
  s1*0^\markup { "triangle" }
  \pattern

  \break

  \override Staff.NoteHead  #'style = #'slash
  s1*0^\markup { "slash" }
  \pattern
}



