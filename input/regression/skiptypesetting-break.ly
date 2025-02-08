\version "2.25.25"

\header {
  texidoc = "The placeholder symbol for material skipped with
@code{skipTypesetting} appears at the end of a line, except in the case of
skipped initial material, when it appears at the beginning.

In this test, the default ellipsis is overridden with the words START, CENTER,
or STOP, depending on its place in the piece.

The first line should begin with the word START and end with a section bar line
after the word CENTER.

The second line should end with a final bar line after the word STOP.
"
}

\layout {
  ragged-right = ##t
}

#(define (x-text-by-dir grob)
  (let ((dir (ly:grob-property grob 'passage-direction CENTER)))
   (cond
    ((= START dir) "START")
    ((= STOP dir) "STOP")
    (else "CENTER"))))

\new Score \with {
  \override StaffEllipsis.text = #x-text-by-dir
} \fixed c' {
  \set Score.skipTypesetting = ##t c1 \set Score.skipTypesetting = ##f
  d1
  \set Score.skipTypesetting = ##t e1 \set Score.skipTypesetting = ##f
  \section \break
  f1
  \set Score.skipTypesetting = ##t g1 \set Score.skipTypesetting = ##f
  \fine
}
