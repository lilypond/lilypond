
\version "2.1.26"
% possible rename.  -gp

\header { texidoc="@cindex Seperate Staccato
You can enter notes and articulations separately, and merge
them into one thread.  In this example, a repead series of staccato dots 
is attached to the notes."
} 

staccatos = \notes { s4-. s-. s-. s s }

music = \notes\relative c' { c4 d e f g  a b c d e }

\score {
   \context Voice <<
     \music
     \repeat unfold 2 \staccatos
   >>
	\paper{raggedright=##t}
}

