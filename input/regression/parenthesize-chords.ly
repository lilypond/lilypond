\version "2.23.4"

\header {
  texidoc = "When @code{\parenthesize} applies to a chord, the
parentheses enclose all notes in the chord.  In tablature one needs to enter
@code{\parenthesize NoteColumn}"
}

\new Voice \relative c {
  \parenthesize <f'' g>
  <<
    { \tweak Parentheses.font-size 0 \parenthesize <ces,, des> }
    { \parenthesize ees' }
    { \tweak Parentheses.font-size -2 \parenthesize <c' e> }
  >>
}

\new TabVoice {
  \override Parentheses.font-size = 0
  \parenthesize <f g>
  \parenthesize NoteColumn <f g>
}
