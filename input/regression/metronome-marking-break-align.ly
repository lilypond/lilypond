\version "2.19.21"

\header {
  texidoc = "@code{\\tempo} marks are aligned with the time signature
or the position of the first note.

By overriding @code{break-align-symbols} the default alignment can be
changed.  If no symbol in @code{break-align-symbols} is present, the
property @code{non-break-align-symbols} determines the alignment.  If
the alignment object is a multi-measure rest, the tempo mark is aligned
with the preceding bar line.
"
}

\layout {
  line-width = 50\mm
}

\relative {
  \tempo "T-first"
  c'1
  \mark \default
  \tempo "T-note"
  c1 \break
  \tempo "T-break"
  c1
  \tempo "T-phantom"
  R1 \break
  \time 8/8
  \tempo "T-time"
  R1
  \override Score.MetronomeMark.break-align-symbols = #'(key-signature)
  \key as \major
  \tempo "T-key"
  c1 \break
  \override Score.MetronomeMark.non-break-align-symbols =
    #'(note-column-interface multi-measure-rest-interface)
  \tempo "T-rest"
  R1
  \tempo "T-rest"
  R1
}
