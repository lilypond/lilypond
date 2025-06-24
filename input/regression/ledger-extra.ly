\version "2.25.27"

\header {
  texidoc = "The @code{ledger-extra} grob property increases
the number of ledger lines drawn, but they are not
drawn on top of staff lines.

Ledgered grobs like @code{Script} or @code{Custos} may be affected differently
from @code{StaffSymbol}."
}


notes =
#(define-music-function (s) (ly:dimension?)
   #{ \override Staff.StaffSymbol.ledger-positions = #'(0 1)
      \override Staff.StaffSymbol.ledger-extra = #s
      \relative c {
        \time 2/4
        \mark \markup \small { "ledger-extra =" #(number->string s) }
        f4 g | a b | c d | e f | g a |
        b4 c | d e | f g | a b | c d |
      } #})

\new Staff { \notes #2 }
\new Staff { \notes #1 }
\new Staff { \notes #0 }
\new Staff { \notes #-1 }
\new Staff { \notes #-2 }


\score {
  \new Staff
    \with { \consists "Custos_engraver" }
    {
      \time 3/2

      \override Staff.StaffSymbol.ledger-extra = 2
      \override Script.padding = 0.8
      \override Script.no-ledgers = ##f
      \override Script.staff-position =
        #(lambda (grob)
           (let* ((par-y (ly:grob-parent grob Y))
                  (staff-pos (ly:grob-property par-y 'staff-position)))
             ((horizontal-script::calc-staff-position (* (sign staff-pos) -2))
               grob)))
      \override Staff.Custos.style = #'mensural

      %% ledgers above
      g'''2\atLeft->
      g'''1\tweak ledger-extra #-2 \atLeft->

      %% no ledgers in default staff
      d''2\atLeft->
      d''1\tweak ledger-extra #-2 \atLeft->

      %% ledgers below
      e2\atLeft->
      e1\tweak ledger-extra #-2 \atLeft->

      \break

      <g g'''>2 2 2

      \override Staff.Custos.ledger-extra = #-2
      \break

      <g g'''>2 2 2
      \fine
    }
  \layout {
    indent = 0
    ragged-right = ##t
  }
}


{
  \mark \markup \small \typewriter "NoteHead.ledger-extra"
  c'''1
  \tweak ledger-extra #4 c'''1
  c'''1
}
