
\header {
  texidoc = "The @code{\parenthesize} function should also work on single
notes (not inside chords), rests, and on whole chords.  Also,
parenthesizing articulations, dynamics and text markup is possible.
On all other music expressions, @code{\parenthesize} does not have
an effect.

Measure 1: Three parenthesized notes (staccato not parenthesized), one
note with staccato in parentheses; Measure 2: Chord and two rests in
parentheses (accent and markup not); Measure 3: note (no parentheses)
with @code{\p} in parentheses, with text in parentheses, and note in
parentheses with @code{\p} not in parentheses, rest (no parentheses);
Measure 4: shows that @code{\parenthesize} does not apply to other
expressions like @code{SequentialMusic}."
}


\paper {
  ragged-right = ##t
}

\version "2.23.4"


\relative c'' {
  % parentheses on single notes (with articulations), inside chord and articulation
  \parenthesize c \parenthesize c-. <\parenthesize c> c-\parenthesize-. |
  % parenthesized rests and whole chords
  \tweak Parentheses.font-size 0 \parenthesize <c e g>4-> \parenthesize r \parenthesize r2^"rest" |
  % parenthesizing dynamics and text markup
  c4-\parenthesize\p c-\parenthesize-"Text" \parenthesize c\p r4 |
  % parenthesizing other music expressions does nothing
  \parenthesize {c4 c-. <c e g> r} \parenthesize|
}
