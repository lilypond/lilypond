
\header {
texidoc = "@cindex Ancient Accidentals
Accidentals are available in different ancient styles, which all
are collected here.
"
}

\version "2.7.39"

\relative c'' {
      \time 5/4
      \override Staff.Accidental  #'style = #'default
      cisis^\markup { \typewriter default } cis c ces ceses 
      \override Staff.Accidental  #'style = #'hufnagel
      cisis^\markup { \typewriter hufnagel } cis c ces ceses 
      \override Staff.Accidental  #'style = #'medicaea
      cisis^\markup { \typewriter medicaea } cis c ces ceses 
      \override Staff.Accidental  #'style = #'vaticana
      cisis^\markup { \typewriter vaticana } cis c ces ceses 
      \override Staff.Accidental  #'style = #'mensural
      cisis^\markup { \typewriter mensural } cis c ces ceses 
}
