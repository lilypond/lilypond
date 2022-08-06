\version "2.23.12"

\layout {
  indent = 0
  ragged-right = ##t
}

staff = \new Staff \fixed c' {
  %% BOL alone
  \caesura

  %% mid-line alone
  d2 \caesura

  %% EOL with measure bar
  e2 \caesura \break |

  %% mid-line with underlying repeat bar
  f2 \caesura \codaMark 1

  %% mid-line with measure bar
  g2 \caesura |

  %% EOL alone
  a2 \caesura \break
  r2 |

  %% EOL with underlying repeat bar
  b2 \caesura \codaMark 1
}

\new PianoStaff << \staff \staff >>
