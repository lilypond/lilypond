
\version "2.14.0"

\header{
  texidoc="
Accidentals work: the second note does not get a sharp. The third and
fourth show forced and cautionary accidentals.
"
}

\layout { ragged-right = ##t }

foo = \relative c''   {   \key as \major dis4 dis dis!^"force" dis? }

<< \foo 
   \context NoteNames \foo
 >>
