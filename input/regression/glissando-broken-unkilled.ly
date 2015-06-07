\version "2.19.21"

\header {
  texidoc = "Broken glissandi anticipate the pitch on the next line."
}

\score {
 \relative {
   \override Glissando.after-line-breaking = ##t
   \override Glissando.breakable = ##t
   c'1\glissando
   \break
   c'1
   \break
   c,1\glissando
   \break
   s2 c'2
 }
 \layout {
   ragged-right = ##t
 }
}

\score {
 \relative {
   \override Glissando.after-line-breaking = ##t
   \override Glissando.breakable = ##t
   c'1\glissando
   \break
   c'1
   \break
   c,1\glissando
   \break
   s2 c'2
 }
}
