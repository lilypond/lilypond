
\header {
    texidoc ="The part combiner stays apart for crossing voices.
"
    }

\version "2.1.36"

vone = \notes \relative a' { g4 g f f e e d d }
vtwo = \notes \relative a' { e4 e f f g g a a }

\score {
   \partcombine \vone \vtwo
   \paper { raggedright = ##t } 
}
 
