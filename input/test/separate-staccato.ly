\header {

texidoc="You can enter notes and articulations separately, and merge
them into one thread.  Here is an example to add repeated staccato dots."

} 

staccatos = \notes { s4-. s-. s-. s s }

music = \notes\relative c' { c4 d e f g  a b c d e }

\score {
   \context Thread=one <
     \music
     \repeat unfold 2 \staccatos
   >
}
