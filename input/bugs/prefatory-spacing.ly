% the space after clef/key is stretched too much.

foo = \notes\relative c''   {   \key as \major; d }

\score {

  < \foo 
   \context NoteNames \foo
  >
}
