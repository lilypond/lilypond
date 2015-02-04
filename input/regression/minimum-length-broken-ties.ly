\version "2.19.16"

\header {
  texidoc = "The following shows the interaction between the
properties @code{minimum-length} and
@code{minimum-length-after-break}.  When @code{minimum-length}
is used alone, both segments of the tie are affected.  The
properties @code{minimum-length-after-break} only affects
the sibling starting a line.  Both properties may be used
together to create independent changes of both siblings.  This
example shows that both properties have an identical effect on
the sibling after the break.
"
}

\layout {
  ragged-right = ##t
}

music = {
  <gis' cis'' dis'' gis''>1~
  \break
  q1
}

{
  % default
  \music

  \once \override Tie.minimum-length = 11
  \music

  \once \override Tie.minimum-length-after-break = 11
  \music

  \once \override Tie.minimum-length = 8
  \once \override Tie.minimum-length-after-break = 11
  \music
}
