\header {
  texidoc = "This regression test combines the @code{StaffSymbol} properties
@code{line-positions}, @code{ledger-positions}, and @code{ledger-extra} to
check ledger line creation behaviour.  The red line indicates the single
staff line used, with the number below showing its vertical position."
}

\version "2.25.27"

#(define ledger-positions '(0 (2 3 4)))

\markup \concat { "ledger line positions: #"
                  #(scm->string ledger-positions) }

$@(map
   (lambda (x)
     #{
       \new Staff \with {
         \omit Clef
         \omit TimeSignature
         \override StaffSymbol.ledger-positions = #ledger-positions
         \override StaffSymbol.color = #red
         \override TextScript.Y-offset = #-7
         \override StaffSymbol.ledger-extra = #x
         instrumentName = #(format #f "ledger-extra: ~a" x)
       } {
         \cadenzaOn
         $@(map
            (lambda (i)
              #{
                \startStaff\stopStaff
                \override Staff.StaffSymbol.line-positions = #(list i)
                <>_\markup \tiny #(number->string i)
                e''4
              #})
            (iota 19 -9 1))
       }
     #})
   (iota 5 -8 4))
