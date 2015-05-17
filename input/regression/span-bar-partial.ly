\version "2.19.21"
\header {
  texidoc = "Span bars can be turned on/@/off on a staff-by-staff basis."
}

\layout {
  \context {
    \Staff
  }
  ragged-right =##t 
}

\relative \new StaffGroup <<
  \new Staff {
    c'1
    \once \override Staff.BarLine.allow-span-bar = ##f
    c1 c1 c1
    \bar "|."
  }
  \new Staff {
    a1 a1
    \once \override Staff.BarLine.allow-span-bar = ##f
    a1 a1
  }
  \new Staff {
    f1 f1 f1 f1
  }
>>



%% Local variables:
%% LilyPond-indent-level:2
%% End:

