\version "2.16.0"

\header {
  texidoc="
Multi-measure rests do not collide with bar lines and clefs.  They
are not expanded when you set @code{Score.skipBars}.  Although the
multi-measure-rest is a Spanner, minimum distances are set to stop it
colliding with bar lines.

Rests over measures lasting longer than 2 wholes use breve rests.  When
more than 10 measures (tunable through @code{expand-limit}) are used
then a different symbol is used.
"
}

\layout {
  \context {
    \Score
    skipBars = ##t
  }
}

{
  \time 3/4
  \key cis \major
  R2.*15
  
  R2.*7
  R2.*9
  R2.*11
  \clef bass
  \time 8/4 R1*2
}
