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
  <>^\markup { "default" }
  \pattern

  \override Staff.NoteHead.style = #'altdefault
  <>^\markup { "altdefault" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'baroque
  <>^\markup { "baroque" }
  \pattern

  \override Staff.NoteHead.style = #'neomensural
  <>^\markup { "neomensural" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'mensural
  <>^\markup { "mensural" }
  \pattern

  \override Staff.NoteHead.style = #'petrucci
  <>^\markup { "petrucci" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'harmonic
  <>^\markup { "harmonic" }
  \pattern

  \override Staff.NoteHead.style = #'harmonic-black
  <>^\markup { "harmonic-black" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'harmonic-mixed
  <>^\markup { "harmonic-mixed" }
  \pattern

  \override Staff.NoteHead.style = #'diamond
  <>^\markup { "diamond" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'cross
  <>^\markup { "cross" }
  \pattern

  \override Staff.NoteHead.style = #'xcircle
  <>^\markup { "xcircle" }
  \pattern

  \break

  \override Staff.NoteHead.style = #'triangle
  <>^\markup { "triangle" }
  \pattern

  \override Staff.NoteHead.style = #'slash
  <>^\markup { "slash" }
  \pattern
}
