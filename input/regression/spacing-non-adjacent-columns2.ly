\version "2.11.51"
\paper {ragged-right = ##t}
\header {
  texidoc = "The spacing engine avoids collisions between non-adjacent columns."
}


foo = \relative c'' {
  g'32[ c,]
}

bar = \relative c {
  s16 <feses ases ceses eses ases ceses eses geses beses deses>4
}

\new PianoStaff {
  <<
    \new Voice \foo
    \new Voice \bar
  >>
}
