\header {
  texidoc = "The vertical positions of ledger lines may be customised by
setting the @code{ledger-positions} property of the @code{StaffSymbol} grob.
The given pattern is repeated.  Ledger lines with positions in sublists are
always shown together: either all or none are shown.  The displayed range of
ledger lines can be modified by setting the @code{ledger-extra} property.

The order of entries in @code{ledger-positions} is not relevant, and neither
is the order in @code{line-positions}; the four shown systems should be
identical." }

\version "2.19.21"

notes =
#(define-music-function (s l) (list? list?)
  #{ \override Staff.StaffSymbol.line-positions = #s
     \override Staff.StaffSymbol.ledger-positions = #l
     \override Staff.StaffSymbol.ledger-extra = #15
     \relative {
       \mark \markup \small \column {
         \concat { "line-positions = #" #(scm->string s) }
         \concat { "ledger-positions = #" #(scm->string l) } }
       g,4 c e b' |
       e4 c'' e g
    } #})

\new Staff \relative { \notes #'(8 7 3 -1 -2 -6) #'(-6 (-2 3) -1) }
\new Staff \relative { \notes #'(8 7 3 -1 -2 -6) #'(-1 -6 (3 -2)) }
\new Staff \relative { \notes #'(-6 -2 -1 3 7 8) #'(-6 (-2 3) -1) }
\new Staff \relative { \notes #'(-6 -2 -1 3 7 8) #'(-1 -6 (3 -2)) }
