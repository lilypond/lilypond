\version "2.19.21"

\header {
    texidoc = "Create compound time signatures. The argument is a Scheme list
of lists. Each list describes one fraction, with the last entry being the
denominator, while the first entries describe the summands in the
enumerator. If the time signature consists of just one fraction,
the list can be given directly, i.e. not as a list containing a single list.
For example, a time signature of (3+1)/8 + 2/4 would be created as
@code{\\compoundMeter #'((3 1 8) (2 4))}, and a time signature of (3+2)/8
as @code{\\compoundMeter #'((3 2 8))} or shorter
@code{\\compoundMeter #'(3 2 8)}.
"
}



\relative {
  \override Staff.TimeSignature.break-visibility = ##(#f #t #t)
  \compoundMeter #'(1 2 3 4 8)
  \repeat unfold 10 c'8 \repeat unfold 20 c16 \break

  \time 3/4
  \repeat unfold 6 c8 \repeat unfold 12 c16 \break

  \compoundMeter #'((1 2 3 4 8) (2 4))
  \repeat unfold 14 c8 \repeat unfold 28 c16 \break

  \compoundMeter #'((1 2 3 4 8) (2 4) (2 3 8))
  \repeat unfold 19 c8 \repeat unfold 38 c16 \break

  \compoundMeter #'(1 2 3 4 8)
  \repeat unfold 10 c8 \repeat unfold 20 c16 \break

  \compoundMeter #'((1 8) (3 8))
  \repeat unfold 4 c8 \repeat unfold 8 c16 \break

  \compoundMeter #'((3 8) (1 8))
  \repeat unfold 4 c8 \repeat unfold 8 c16 \break

  \time 4/4
  \repeat unfold 8 c8 \repeat unfold 16 c16 \break

  \bar"|."
}
