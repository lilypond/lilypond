\version "2.25.23"

\header {
  texidoc = "New entries may be added to the @code{default-script-alist}.

They may contain a setting for @code{staff-position} for an offset in Y-axis
direction, if used horizontally.  It is possible to customize @code{Y-offset}
or @code{staff-position} by using @code{\\override} or @code{\\tweak}.
The @code{segno-coda} articulation defined here prints a segno bottom/left and a
coda top/right.  The horizontal segno should be printed on the second, third,
and fourth staff line; the horizontal coda at the second staff line.

The @code{segno-coda-II} also defined here has additional default settings for
@code{side-axis} and @code{direction}.
As a result these scripts need a different set of @code{\\override} or
@code{\\tweak} calls, if applied against their @code{side-axis}.

In this test both staves should look equal."
}

#(define my-script-alist
  (cons*
    `(segno-coda
      . (
         (script-stencil . (feta . ("segno" . "coda")))
         (padding . 0.7)
         (avoid-slur . around)
         (staff-position . ,(horizontal-script::calc-staff-position -2))
         (side-axis . ,Y)
         (direction . ,UP)
        ))
    `(segno-coda-II
      . (
         (script-stencil . (feta . ("segno" . "coda")))
         (padding . 0.7)
         (avoid-slur . around)
         (side-axis . ,X)
         (direction . ,LEFT)
         (staff-position . ,(horizontal-script::calc-staff-position -2))
         (direction . ,LEFT)
        ))
     default-script-alist))

segnoCoda = #(make-articulation 'segno-coda)
segnoCodaII = #(make-articulation 'segno-coda-II)

\layout {
  \context {
    \Score
    scriptDefinitions = #my-script-alist
  }
  \context {
    \Voice
    \override Script.padding = #0.5
  }
}

{
  \textMark \markup \bold "bottom/top"
  b''2 -> -\prall -\mordent ^\segnoCoda
  b -> -\prall -\mordent _\segnoCoda

  \textMark \markup \bold "left/right"
  \override Script.side-axis = #X
  \override Script.direction = #LEFT

  b'2 -> -\segnoCoda
  b'2 -> -\tweak Y-offset #0 -\segnoCoda
  b'2 -> -\tweak staff-position #2 -\segnoCoda

  \override Script.direction = #RIGHT
  b' -\segnoCoda
}

{
  \textMark \markup \bold "bottom/top"
  b''2 -> -\prall -\mordent \tweak side-axis #Y ^\segnoCodaII
  b -> -\prall -\mordent \tweak side-axis #Y _\segnoCodaII

  \textMark \markup \bold "left/right"

  b'2 \atLeft -> -\segnoCodaII
  b'2 \atLeft -> -\tweak Y-offset #0 -\segnoCodaII
  b'2 \atLeft -> -\tweak staff-position #2 -\segnoCodaII

  \override Script.direction = #RIGHT
  b' -\segnoCodaII
}
