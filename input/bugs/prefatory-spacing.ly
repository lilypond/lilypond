\header { texidoc= "Spacing after clef/key should be stretched
equidistantly for large stretching, but should stay clear of prefatory
matter for small stretching. Support in the spacing engine for
separate spring constants for stretching/shrinking is needed"; 
}

foo = \notes\relative c''   {   \key as \major; f }

\score {

  < \foo 
  >
}
