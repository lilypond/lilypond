
\header{
texidoc="
Accidentals work: the second note does not get a sharp. The third and
fourth show forced and courtesy accidentals.
";
}

foo = \notes\relative c''   {   \key as \major; dis4 dis dis! dis? }

\score {

  < \foo 
   \context NoteNames \foo
  >
}
