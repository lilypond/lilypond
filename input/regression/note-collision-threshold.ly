\version "2.19.34"

\header {
  texidoc = "Whether simultaneous notes are identified as
  vertically colliding or not depends on the value of the
  @code{note-collision-threshold} property of the @code{Stem}
  grob (for notes in the same voice) and the @code{NoteCollision}
  grob (for notes in different voices)."
}

music = <<
  \new Voice \relative {
    \voiceOne
    <c' d>4 <d e> <f g> <g a>
    e g g a

  }
  \new Voice \relative {
    \voiceTwo
    s4 s s s
    d' f a b
  }
>>

customizations = \with {
  staffLineLayoutFunction = #ly:pitch-semitones
  \override StaffSymbol.staff-space = #0.7
  \override StaffSymbol.line-positions = #'(-4 0 4)
}


\markup "collisions"

\new Staff \with {
  \customizations
}
\music

\markup "collisions prevented"

\new Staff \with {
  \customizations
  \override Stem.note-collision-threshold = #2
  \override NoteCollision.note-collision-threshold = #2
}
\music
