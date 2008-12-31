
\header {

  texidoc = "By setting properties in @code{NonMusicalPaperColumn}, vertical
spacing of alignments can be adjusted per system.

By setting @code{alignment-extra-space} or
@code{fixed-alignment-extra-space} an individual system may be
stretched vertically.

For technical reasons, @code{overrideProperty} has to be used for
setting properties on individual object. @code{\override} in a
@code{\context} block may still be used for global overrides.

"

}

\version "2.12.0"

#(set-global-staff-size 13)

\relative c''
\new StaffGroup <<
  \new Staff {
    c1\break 
    c\break c\break
  }
  \new Staff { c1 c c }
  \new PianoStaff <<
    \new Voice  {
      \set PianoStaff.instrumentName = #"piano"
      \set PianoStaff.shortInstrumentName = #"pn"
      c1_"normal"
      
      \overrideProperty
      #"Score.NonMusicalPaperColumn"
      #'line-break-system-details
      #'((fixed-alignment-extra-space . 15))
      c_"fixed-aligment-extra-space"

      \overrideProperty
      #"Score.NonMusicalPaperColumn"
      #'line-break-system-details
      #'((alignment-extra-space . 15))
      c_"aligment-extra-space"
    }
    { c1 c c }
  >>
>>

