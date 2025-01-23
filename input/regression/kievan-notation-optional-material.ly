\version "2.25.25"

\header {
  texidoc = "The brackets for optional material in a @code{KievanStaff} have
specialized default values."
}

#(ly:set-option 'warning-as-error #t)

#(set-global-staff-size 40)

\layout {
  ragged-right = ##t
}

\new KievanStaff \with {
  \remove Clef_engraver
} \new KievanVoice \fixed c' {
  \set Timing.timing = ##f
  \override Score.SpacingSpanner.packed-spacing = ##t
  e1
  \startOptionalMaterial
  f2 g1
  \stopOptionalMaterial
  \caesura
  e2
}
