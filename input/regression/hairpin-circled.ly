\version "2.25.12"

\header {

  texidoc = "Hairpins can have circled tips.  A decrescendo del niente
followed by a crescendo al niente should only print one circle.
If a decrescendo ends at first note of a new line the circle is printed at the
end of the previous line or in the new line, depending on the setting of
@code{after-line-breaking}."

}

\layout { ragged-right = ##t }

mus = \relative {
  \override Hairpin.circled-tip = ##t
  c''1\< d\! d\> c\!
  \override Hairpin.to-barline = ##f
  e\> c\< d\! \break
  c\< \break
  e d\! c\> \break
  e2 f\! d1\> \break
  c2\!
  \override Hairpin.after-line-breaking = #'()
  d\> \break
  c1\!
}

<<
  \new Staff \mus
  \new Staff { \override Hairpin.stencil = #flared-hairpin \mus }
>>

