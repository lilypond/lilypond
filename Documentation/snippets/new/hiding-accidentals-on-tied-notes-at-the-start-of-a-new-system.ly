\version "2.14.0"
\header {
  lsrtags = "pitches"
  texidoc = "This shows how to hide accidentals on tied notes at the beginning of a
new system."
  doctitle = "Hiding accidentals on tied notes at the beginning of a new system"
}

\relative c'' {
  \override Accidental #'hide-tied-accidental-after-break = ##t
  cis1~ cis~
  \break
  cis
}

