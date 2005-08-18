
\version "2.6.0"

\header{
  texidoc="
Accidentals work: the second note does not get a sharp. The third and
fourth show forced and courtesy accidentals.
"
}

\layout { raggedright = ##t }

foo = \relative c''   {   \key as \major dis4 dis dis!^"force" dis? }

<< \foo 
   \context NoteNames \foo
 >>
