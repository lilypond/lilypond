\version "2.1.23"
\header { texidoc = "Tests a collision between multimeasure rests in
different voices. " }
\score {
  \context Staff \notes <<
    \new Voice {
      \override MultiMeasureRest  #'staff-position = #3
      R1
    }
    \new Voice {
      \override MultiMeasureRest  #'staff-position = #-3
      R1
    }
  >>
  \paper {
    \translator {
      \StaffContext
      \remove Multi_measure_rest_engraver
      \remove Bar_engraver
    }
    \translator {
      \VoiceContext
      \consists Multi_measure_rest_engraver
      \consists Bar_engraver
    }
	raggedright = ##t
  }
}

