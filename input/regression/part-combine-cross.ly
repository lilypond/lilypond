
\header {
    texidoc ="The part combiner stays apart for crossing voices.
"
    }

\version "2.19.21"

vone =  \relative { g'4 g f f e e d d }
vtwo =  \relative { e'4 e f f g g a a }
\layout { ragged-right = ##t } 

\partcombine \vone \vtwo
 
