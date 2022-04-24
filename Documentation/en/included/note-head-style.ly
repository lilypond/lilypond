\version "2.19.0"

\header {
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

  \context {
    \Score
    \remove Bar_number_engraver
  }
}

pattern = <<
  \new Voice {
    \override Stem.direction = #UP
    e'4 2. 1 \breve*1/2 \longa*1/4 \bar "||"
  }
  \new Voice {
    \override Stem.direction = #DOWN
    a4  2. 1 \breve*1/2 \longa*1/4 \bar "||"
  }
>>


\transpose c c {
  \clef C

  \override Staff.NoteHead.style = #'default
  s1*0^\markup { "default" }
  \pattern

  \override Staff.NoteHead.style = #'altdefault
  s1*0^\markup { "altdefault" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'baroque
  s1*0^\markup { "baroque" }
  \pattern

  \override Staff.NoteHead.style = #'neomensural
  s1*0^\markup { "neomensural" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'mensural
  s1*0^\markup { "mensural" }
  \pattern

  \override Staff.NoteHead.style = #'petrucci
  s1*0^\markup { "petrucci" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'harmonic
  s1*0^\markup { "harmonic" }
  \pattern

  \override Staff.NoteHead.style = #'harmonic-black
  s1*0^\markup { "harmonic-black" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'harmonic-mixed
  s1*0^\markup { "harmonic-mixed" }
  \pattern

  \override Staff.NoteHead.style = #'diamond
  s1*0^\markup { "diamond" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'cross
  s1*0^\markup { "cross" }
  \pattern

  \override Staff.NoteHead.style = #'xcircle
  s1*0^\markup { "xcircle" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'triangle
  s1*0^\markup { "triangle" }
  \pattern

  \override Staff.NoteHead.style = #'slash
  s1*0^\markup { "slash" }
  \pattern
}
