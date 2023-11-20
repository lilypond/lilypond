\version "2.19.21"

\header {
  texidoc = "Figured bass extender lines shall be broken when a figure has a
  different alteration, augmentation, or diminishment."
}

<<
  \new Voice \relative {
    c'8 c c c
    c c
  }
  \figures {
    \bassFigureExtendersOn
    <6 4 3>8 <6\\ 4! 3!> <6 4- 3+> <6/ 4\+ 3>
    <6 4\! 3+> <6\+ 4\+ 3++>
  }
>>
