\version "1.7.18"
\header { texidoc = "Tests a collision between multimeasure rests in
different voices. " }
\score {
  \context Staff \notes <
    \context Voice=i {
      \property Voice.MultiMeasureRest \override #'staff-position = #3
      R1
    }
    \context Voice=ii {
      \property Voice.MultiMeasureRest \override #'staff-position = #-3
      R1
    }
  >
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

