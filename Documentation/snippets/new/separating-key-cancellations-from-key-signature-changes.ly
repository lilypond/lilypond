\version "2.25.35"

\header {
  categories = "Pitches, Scheme, Tweaks and overrides"

  texidoc = "
By default, the accidentals used for key cancellations are placed
adjacent to those for key signature changes. This behavior can be
changed by overriding the @code{break-align-orders} property of the
@code{BreakAlignment} grob.

If you look up the definition of the @code{break-alignment-interface} in
LilyPond's
@uref{https://lilypond.org/doc/v2.24/Documentation/internals/break_002dalignment_002dinterface,
Internals Reference}, you get the following list of available break-align
symbols:

@example
ambitus
breathing-sign
clef
cue-clef
cue-end-clef
custos
key-cancellation
key-signature
left-edge
signum-repetitionis
staff-bar
staff-ellipsis
time-signature
@end example

From this list, we find that we want to move @code{key-cancellation} so
that it comes before @code{staff-bar}. This is accomplished with the
@code{\\breakAlignInsert} function."

  doctitle = "Separating key cancellations from key signature changes"
} % begin verbatim


music = { \key es \major d'1 \bar "||"
          \key a \major d'1 }

{ <>^\markup "default"
  \music }

{ <>^\markup "cancellation first"
  \breakAlignInsert key-cancellation before staff-bar
  \music }
