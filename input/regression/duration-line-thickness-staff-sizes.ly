\version "2.23.9"

\header {
  texidoc = "The visible thickness of a duration line is adjusted properly
according to the staff space for all styles, like for long compressed
@code{MultiMeasureRest}.
Changes in @code{StaffSymbol.thickness} are disregarded."
}

mus = {
  \override DurationLine.bound-details.right.end-style = #'hook
  b'1\-
  \override DurationLine.bound-details.right.end-style = #'arrow
  \override DurationLine.style = #'line
  b'\-
  \override DurationLine.style = #'dashed-line
  b'\-
  \override DurationLine.style = #'dotted-line
  b'\-
  \override DurationLine.style = #'zigzag
  b'\-
  \override DurationLine.style = #'trill
  b'\-
  \override DurationLine.style = #'none
  b'\-
  \compressEmptyMeasures
  R1*20
}

\score {
 <<
  \new StaffGroup
    \with {
      instrumentName =
        \markup \column {
          "changed"
          "StaffSymbol"
          "staff-space"
        }
    }
    <<
      \new Staff
        \with {
          \override StaffSymbol.staff-space = #(magstep 6)
        }
        {
          <>^
            \markup
            \override #'(line-width . 70)
            \justify-string
              #"With changed staff-space (and unchanged StaffSymbol.thickness),
              DurationLine is adjusted nicely.  For trill style one would need
              to set grob.font-size (here done) or context.fontSize
              additionally."
          \override DurationLine.font-size = #6
          \mus
        }
      \new Staff
        \mus
      \new Staff
        \with {
          \override StaffSymbol.staff-space = #(magstep -6)
        }
        {
          \override DurationLine.font-size = #-6
          \mus
        }
    >>

  \new StaffGroup
    \with { instrumentName = "\\magnifyStaff" }
    <<
      \new Staff
        \with { \magnifyStaff #(magstep 6) }
        {
          <>^\markup \fontsize #-6"\\magnifyStaff"
          \mus
        }
      \new Staff
        \mus
      \new Staff
        \with { \magnifyStaff #(magstep -6) }
        \mus
    >>

  \new StaffGroup
    \with {
      instrumentName =
        \markup \column {
          "changed"
          "StaffSymbol"
          "thickness"
        }
    }
    <<
      \new Staff
        \with { \override StaffSymbol.thickness = #(magstep 6) }
        {
         <>^
           \markup \column {
            "With unchanged staff-space and modified StaffSymbol.thickness,"
            "DurationLine keeps unchanged."
           }
         \mus
        }
      \new Staff
        \mus
      \new Staff
        \with { \override StaffSymbol.thickness = #(magstep -6) }
        \mus
    >>
 >>
 \layout {
   indent = 15
   \context {
     \StaffGroup
     \override InstrumentName.font-size = #-2
   }
   \context {
     \Voice
     \consists "Duration_line_engraver"
     %% If broken and end-style is set:
     \override DurationLine.after-line-breaking = ##t
     \override DurationLine.minimum-length-after-break = 10
   }
 }
}
