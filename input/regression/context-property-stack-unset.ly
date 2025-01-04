\version "2.25.24"

\header {
  texidoc = "@code{\\pushContextProperty} can save the state of a property that
is not currently set.  @code{\\popContextProperty} restores it to unset.  The
beaming of the lower voice should follow the beaming of the upper voice in
measures 1 and@tie{}3; it should differ in measure@tie{}2."
}

#(ly:set-option 'warning-as-error #t)

\new Staff <<
  {
    \time 2/4
    \set Score.autoBeaming = ##f
    \skip 8*6
    \set Score.autoBeaming = ##t
  }
  \new Voice \with \voiceOne {
    \repeat unfold 12 c''8 % uses Score.autoBeaming
  }
  \new Voice \with \voiceTwo {
    \repeat unfold 4 a'8 % uses Score.autoBeaming (#f)
    \pushContextProperty Voice.autoBeaming ##t
    \repeat unfold 4 a'8 % uses Voice.autoBeaming (#t)
    \popContextProperty Voice.autoBeaming
    \repeat unfold 4 a'8 % uses Score.autoBeaming (#t) again
  }
>>
