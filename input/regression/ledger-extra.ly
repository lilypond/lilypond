\version "2.19.36"

\header {
  texidoc = "The @code{ledger-extra} grob property increases
the number of ledger lines drawn, but they are not
drawn on top of staff lines.

Ledgered grobs like @code{Script} or @code{Custos} may be affected differently
from @code{StaffSymbol}."
}

notes = \relative {
  \time 6/8
  a4. b | c d | e f |
  g4. a | b c | d e |
  f4. g | a b |
}

\new Staff {
  \notes
}

\new Staff {
  \override Staff.StaffSymbol.ledger-extra = 1
  \notes
}

\new Staff {
  \override Staff.StaffSymbol.ledger-extra = 2
  \notes
}


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
