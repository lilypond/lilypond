\version "2.11.51" \header{


    texidoc=" Both noteheads and rests can have dots.  
    Augmentation dots should never be printed on a staff line,
but rather be shifted vertically. They should go up, but in case of
multiple parts, the down stems have down shifted dots.  In case of
chords, all dots should be in a column.  The dots follow the shift
of rests when avoiding collisions.

The priorities to print the dots are (ranked in importance):

@itemize @bullet
@item keeping dots off staff lines,
@item keeping dots close to their  note heads,
@item moving dots in the direction specified by the voice,
@item moving dots up.
@end itemize

"
}



\context Voice \relative c'' {
  \time 6/8
  d4. g,,
  \stemDown
  <b'' c d e>4.  <f g a b>
  <f a c> <e a c> <b f' c' g'>
  
  
  <<
    { f  <b c> r4.  }\\
    { b, <a b> r4. }
  >>
  
  
}

