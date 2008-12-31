
\header {
    texidoc ="The part combiner detects a2, solo1 and solo2, and prints
texts accordingly.

"

    
    }


\version "2.12.0"

\layout { ragged-right= ##t }

vone =  \relative a' { R1 a2    r4 r a a a a }
vtwo =  \relative a' { R1 f4 f4 f4 f f f a a  }
comm = { s1 s2 s4_\markup { \small "expect: solo 2" }
	 s4 s2 s4_\markup { \small "expect: a2"  } s4 } 
\new Staff <<
    \partcombine \vone \vtwo
    \comm
>>
 
