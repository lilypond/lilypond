\version "2.25.23"

\header {
  texidoc = "New entries may be added to the @code{default-script-alist}.
They may contain a setting for @code{staff-position} for an offset in Y-axis
direction, if used horizontally.  It is possible to customize @code{Y-offset}
or @code{staff-position} by @code{override} or @code{tweak}.  The here defined
@code{segno-coda} articulation prints a segno bottom/left and a coda top/right.
The horizontal segno should be printed on second, third and fourth staff-line.
The horizontal coda at second staff-line."
}

#(define my-script-alist
  (cons
    `(segno-coda
      . (
         (script-stencil . (feta . ("segno" . "coda")))
         (padding . 0.7)
         (avoid-slur . around)
         (staff-position . ,(horizontal-script::calc-staff-position -2))
         (direction . ,LEFT)
        ))
     default-script-alist))

segnoCoda = #(make-articulation 'segno-coda)

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
