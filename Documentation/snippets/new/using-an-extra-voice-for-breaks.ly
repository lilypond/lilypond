\version "2.25.35"

\header {
  categories = "Specific notation, Workaround"

  texidoc = "
Often it is easier to manage line and page-breaking information by
keeping it separate from the music by introducing an extra voice
containing only skips along with the @code{\\break},
@code{\\pageBreak}, and other layout information.

This pattern becomes especially helpful when overriding
@code{line-break-system-details} and the other useful but long
properties of the @code{NonMusicalPaperColumn} grob.
"

  doctitle = "Using an extra voice for breaks"
} % begin verbatim


music = \relative c'' { c4 c c c }

\score {
  \new Staff <<
    \new Voice {
      s1*2 \break
      s1*3 \break
      s1*4 \break
      s1*5 \break
    }
    \new Voice {
      \*2 \music
      \*3 \music
      \*4 \music
      \*5 \music
    }
  >>
}

\paper {
  indent = 0
  line-width = 140\mm
  ragged-right = ##t
}
