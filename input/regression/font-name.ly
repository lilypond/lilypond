\version "2.5.19"

\header {

    texidoc = "Other fonts can be used by setting @code{font-name} for
the appropriate object. The string should be a Pango font description
without size specification."


}

\layout { raggedright = ##t }


{
  \override Staff.TimeSignature  #'font-name = #"Times"
  \time 3/4
  \set Score.skipBars = ##t

				% use font-name putri8r for Utopia Italic :
  
  \override Staff.MultiMeasureRestText #'font-name = #"LuxiMono"
  R1*21^"Rest in LuxiMono"

  c'1_\markup {
    \override #'(font-name . "Utopia Bold")
      \override #'(font-size . 4)
        { This text is in large Utopia Bold }
  }
}

