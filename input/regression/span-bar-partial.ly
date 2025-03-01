\version "2.25.25"

\header {
  texidoc = "Span bars can be turned on/@/off on a staff-by-staff basis.

Bar@tie{}2 should have no span bar between the top and the middle staves.
Bar@tie{}3 should have no span bar between the middle and the bottom staves.
There should be a fermata at the end of every bar line where no span bar is
attached."
}

#(ly:set-option 'warning-as-error #t)

testCaesura = { \caesura ^\fermata _\fermata }

\new StaffGroup \with {
  caesuraType = #'((underlying-bar-line . ""))
} \fixed c' <<
  \new Staff {
    b1 \testCaesura
    \once \override Staff.BarLine.allow-span-bar = ##f
    b1 \testCaesura
    b1 \testCaesura
    b1 \testCaesura
    \fine
  }
  \new Staff {
    a1 \testCaesura
    a1 \testCaesura
    \once \override Staff.BarLine.allow-span-bar = ##f
    a1 \testCaesura
    a1 \testCaesura
    \fine
  }
  \new Staff {
    g1 \testCaesura
    g1 \testCaesura
    g1 \testCaesura
    g1 \testCaesura
    \fine
  }
>>
