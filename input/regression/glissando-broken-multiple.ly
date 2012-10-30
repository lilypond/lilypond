\header {
  texidoc = "When broken, glissandi can span multiple lines."

}
\version "2.17.6"
\paper {
  ragged-right = ##t
}

\relative c'' {
  \override Glissando.breakable = ##t
  \override Glissando.after-line-breaking = ##t
  d1\glissando
  \break s1
  \break s1
  \break s1
  c,1^\ff\trill
  % Subsequent glissando prints correctly instead of
  % using the Y positions from the previous one.
  a'1\glissando
  \break s1
  \break s1
  \break s1
  e1^\ff\trill
}
