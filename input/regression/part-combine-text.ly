
\header {
    texidoc ="The new part combiner:

Detect a2, solo1, solo2  and print texts accordingly.

"

    
    }


\version "2.1.18"

vone = \notes \relative a' { R1 a2    r4 r a a a a }
vtwo = \notes \relative a' { R1 f4 f4 f4 f f f a a  }

\score {
   \partcombine \vone \vtwo
}
 
