\version "2.19.21"
\paper {ragged-right = ##t}
\header {
  texidoc = "The spacing engine avoids collisions between non-adjacent columns."
}


foo = \relative {
  g''32[ c,]
}

bar = \relative {
  c16 <feses ases ceses eses ases ceses eses geses beses deses>4
}

\new PianoStaff {
  <<
    \new Voice \foo
    \new Voice \bar
  >>
}
