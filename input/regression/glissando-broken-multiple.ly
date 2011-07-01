\header {
  texidoc = "When broken, glissandi can span multiple lines."

}
\version "2.15.0"
\paper {
  ragged-right = ##t
}

\relative c'' {
  \override Glissando #'breakable = ##t
  \override Glissando #'after-line-breaking = ##t
  d1\glissando
  \break s1
  \break s1
  \break s1
  c,1^\ff\trill
}
