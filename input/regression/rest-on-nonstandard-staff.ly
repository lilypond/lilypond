\version "2.17.6"

\header {
  texidoc = "half rests should lie on a staff line, whole rests should hang
  from a staff line by default even for non-standard staves, except when
  the position is set by pitch."
}


\layout {
  ragged-right = ##t
  indent = 0.0
}

mus = {
  r2
  b\rest
  c'\rest d'\rest e'\rest f'\rest g'\rest a'\rest b'\rest
  c''\rest d''\rest e''\rest f''\rest g''\rest a''\rest b''\rest
  r1
  b\rest
  c'\rest d'\rest e'\rest f'\rest g'\rest a'\rest b'\rest
  c''\rest d''\rest e''\rest f''\rest g''\rest a''\rest b''\rest
  r\breve
  b\rest
  c'\rest d'\rest e'\rest f'\rest g'\rest a'\rest b'\rest
  c''\rest d''\rest e''\rest f''\rest g''\rest a''\rest b''\rest
  r\longa
  b\rest
  c'\rest d'\rest e'\rest f'\rest g'\rest a'\rest b'\rest
  c''\rest d''\rest e''\rest f''\rest g''\rest a''\rest b''\rest
  <<
    { r2 r2 r1 r\breve r\longa }
    \\
    { r2 r2 r1 r\breve r\longa }
  >>
}

\new StaffGroup <<
  \new Staff {
    \mus
  }

  \new Staff {
    \override Staff.StaffSymbol.line-positions = #'(-4 -2 0 2)
    \mus
  }

  \new Staff {
    \override Staff.StaffSymbol.line-count = #4
    \mus
  }

  \new Staff {
    \override Staff.StaffSymbol.line-positions = #'(-4 -2 1 5)
    \mus
  }

  \new Staff {
    \stopStaff
    \mus
  }
>>
