

\header{
  texidoc = "If a floating grace spacing section attaches to a note
across a line break, it gets attached to the end of line."
}


\version "2.11.5"

\new Score \with {
  \override SpacingSpanner #'strict-grace-spacing = ##t
  \override PaperColumn #'used = ##t 
} <<

  \new Staff <<

    \new Voice {
      \time 2/4    s2
      \time 2/4    s2 \break
      \time 2/4    s2
    }

    \new Voice {
      b'4
      \afterGrace c''4 {b'16}
      s4
      s4
      c''4
    }
    \new Voice {
      c'4
      c'8.
      r16
      c'4
      c'4
      c'4
    }
  >>
>>

