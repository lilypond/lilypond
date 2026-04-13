\version "2.25.35"

\header {
    texidoc = "Create complex time signatures. The argument is a Scheme list
of lists. Each list describes one fraction, with the last entry being the
denominator, while the first entries describe the summands in the
enumerator. If the time signature consists of just one fraction,
the list can be given directly, i.e. not as a list containing a single list.
For example, a time signature of (3+1)/8 + 2/4 would be created as
@code{\\timeAbbrev #'((3 1 8) (2 4))}, and a time signature of (3+2)/8
as @code{\\timeAbbrev #'((3 2 8))} or shorter
@code{\\timeAbbrev #'(3 2 8)}.
"
}



\relative {
  \override Staff.TimeSignature.break-visibility = ##(#f #t #t)
  \timeAbbrev #'(1 2 3 4 8)
  \*10 c'8 \*20 c16 \break

  \time 3/4
  \*6 c8 \*12 c16 \break

  \timeAbbrev #'((1 2 3 4 8) (2 4))
  \*14 c8 \*28 c16 \break

  \timeAbbrev #'((1 2 3 4 8) (2 4) (2 3 8))
  \*19 c8 \*38 c16 \break

  \timeAbbrev #'(1 2 3 4 8)
  \*10 c8 \*20 c16 \break

  \timeAbbrev #'((1 8) (3 8))
  \*4 c8 \*8 c16 \break

  \timeAbbrev #'((3 8) (1 8))
  \*4 c8 \*8 c16 \break

  \time 4/4
  \*8 c8 \*16 c16 \break

  \bar"|."
}
