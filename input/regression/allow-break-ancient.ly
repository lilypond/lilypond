\version "2.23.12"

\header {
  texidoc = "By default, certain staff contexts for ancient music do
not forbid line breaks between bar lines.  The output should have a
break at a point without a bar line."
}

music = \repeat unfold 10 {
  c'4
  4
  4
  4 \noBreak
  \bar "!"
}

\new StaffGroup \with { \unset systemStartDelimiter } <<
  \new KievanStaff << \music >>
  \new MensuralStaff << \music >>
  \new PetrucciStaff << \music >>
  \new VaticanaStaff << \music >>
>>
