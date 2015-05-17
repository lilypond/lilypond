\version "2.19.21"
\header {
    texidoc = "There are three different kinds of grace types: the base 
grace switches to smaller type, the appoggiatura inserts also a slur, and the
acciaccatura inserts a slur and slashes the stem." 
    }

\layout {
    ragged-right = ##t
}

\relative {
    c''4 \grace { d8 }  c4
    \appoggiatura { d8 } c
    \acciaccatura { d } c
}
    
 
