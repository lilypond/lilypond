
\header {
texidoc = "@cindex Ancient Accidentals
Accidentals are available in different ancient styles, which all
are collected here.
"
}

\version "2.12.0"

\relative c'' {
      \time 3/4
      % FIXME: standard vs default, alteration-FOO vs FOO-alteration
      \override Staff.Accidental #'glyph-name-alist = #standard-alteration-glyph-name-alist
      cis^\markup { \typewriter default } c ces  

      \override Staff.Accidental #'glyph-name-alist = #alteration-hufnagel-glyph-name-alist
      cis^\markup { \typewriter hufnagel } c ces  

      \override Staff.Accidental #'glyph-name-alist = #alteration-medicaea-glyph-name-alist
      cis^\markup { \typewriter medicaea } c ces  

      \override Staff.Accidental #'glyph-name-alist = #alteration-vaticana-glyph-name-alist
      cis^\markup { \typewriter vaticana } c ces  

      \override Staff.Accidental #'glyph-name-alist = #alteration-mensural-glyph-name-alist
      cis^\markup { \typewriter mensural } c ces  
}
