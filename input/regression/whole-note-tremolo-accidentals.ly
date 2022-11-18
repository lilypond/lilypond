\version "2.25.0"

\header {
  texidoc = "Whole note tremolos (and longer) don't collide with
accidentals."
}

{
  \repeat tremolo 16 { c'32 es' }
  \repeat tremolo 16 { a'32 cis'' } 
  \repeat tremolo 16 { a'32 gisis' }
  <<  
    \repeat tremolo 16 { a''32 bes'' }
    \\  
    \repeat tremolo 16 { f'32 d'! }
  >>  
}

{  
  \repeat tremolo 16 { c'32 <es' es''> }
  \repeat tremolo 16 { a'32 <f' cis''> }
  \repeat tremolo 16 { a'32 <disis' ges''> }
}

{  
  \time 4/2
  \repeat tremolo 16 { c'16 es' }
  \repeat tremolo 16 { a'16 cis'' }
  \repeat tremolo 16 { a'16 gisis' }
  <<  
    \repeat tremolo 16 { a''16 bes'' }
    \\  
    \repeat tremolo 16 { f'16 d'! }
  >>  
}

{ 
  \time 4/2
  \repeat tremolo 16 { c'16 <es' es''> }
  \repeat tremolo 16 { a'16 <f' cis''> }
  \repeat tremolo 16 { a'16 <disis' ges''> }
}

{  
  \time 4/1
  \repeat tremolo 16 { c'8 es' }
  \repeat tremolo 16 { a'8 cis'' }
  \repeat tremolo 16 { a'8 gisis' } 
  <<  
    \repeat tremolo 16 { a''8 bes'' }
    \\  
    \repeat tremolo 16 { f'8 d'! }
  >>  
}

{ 
  \time 4/1
  \repeat tremolo 16 { c'8 <es' es''> }
  \repeat tremolo 16 { a'8 <f' cis''> }  
  \repeat tremolo 16 { a'8 <disis' ges''> }
}
