
\header {
    texidoc ="The new part combiner detects a2, solo1 and solo2, and prints i
texts accordingly.

"

    
    }


\version "2.3.4"

vone =  \relative a' { R1 a2    r4 r a a a a }
vtwo =  \relative a' { R1 f4 f4 f4 f f f a a  }

\score {
   \partcombine \vone \vtwo
}
 
