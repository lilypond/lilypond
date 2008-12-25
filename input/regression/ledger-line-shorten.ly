\header {
    texidoc = "Ledger lines are shortened when they are very close. This ensures
that ledger lines stay separate."
}
\version "2.12.0"

\layout {
    ragged-right = ##t
}

\relative {
\time 2/4 

c4
b
c4
b
a g
c32[ b c d]
b[ c d c]
b[ d d b]
b[ e e b]
c[ d d c]
c[ e e c]

     

}  
