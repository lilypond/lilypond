
\version "2.19.21"

\header{
  texidoc="
Accidentals work: the second note does not get a sharp. The third and
fourth show forced and cautionary accidentals.
"
}

\layout { ragged-right = ##t }

foo = \relative   {   \key as \major dis''4 dis dis!^"force" dis? }

<< \foo 
   \context NoteNames \foo
 >>
