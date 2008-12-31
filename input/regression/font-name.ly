\version "2.12.0"

\header {

    texidoc = "Other fonts can be used by setting @code{font-name} for
the appropriate object. The string should be a Pango font description
without size specification."


}

\layout { ragged-right = ##t }


{
  \override Staff.TimeSignature  #'font-name = #"Times New Roman"
  \time 3/4
  \set Score.skipBars = ##t
  \override Staff.MultiMeasureRestText #'font-name = #"LuxiMono"
  R1*21^"Rest in LuxiMono"

  c'1_\markup {
    \override #'(font-name . "Vera Bold")
      \override #'(font-size . 4)
        { This text is in large Vera Bold }
  }
}

