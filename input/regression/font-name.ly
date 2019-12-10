\version "2.17.6"

\header {

    texidoc = "Other fonts can be used by setting @code{font-name} for
the appropriate object. The string should be a Pango font description
without size specification."


}

\layout { ragged-right = ##t }


{
  \override Score.PaperColumn.keep-inside-line = ##f

  % A comma is required
  % for font name "Times New Roman"'s explicit termination.
  % If there is no comma, Pango interpret "Times New Roman" as
  % "Times New" family with "Roman" style.
  \override Staff.TimeSignature.font-name = "Times New Roman,"
  \time 3/4
  \set Score.skipBars = ##t
  \override Staff.MultiMeasureRestText.font-name = "Luxi Mono"
  R1*21^"Rest in Luxi Mono"

  c'1_\markup {
    \override #'(font-name . "Bitstream Vera Sans, Bold")
      \override #'(font-size . 4)
        { This text is in large Vera Bold }
  }
}

