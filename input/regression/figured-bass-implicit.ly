\version "2.19.21"

\header {
  texidoc = "Implicit bass figures are not printed, but they do get
extenders."
}

<<
  \new Voice \relative {
    \time 3/4
    c''^"normal" c c |
    c^"extenders" c c |
    c_"implicit" c c
  }
  \figures {
    <3 6!>
    <3 4+>
    r |
    \set useBassFigureExtenders = ##t
    <3 6!>
    <3 4+>
    r |
    \set useBassFigureExtenders = ##t
    \set implicitBassFigures = #'(3)
    <3 6!>
    <3 4+>
    r
  }
>>
