\header{
texidoc="
Chord tremolos look like beams, but are a kind of repeat symbol.
To avoid confusion, chord tremolo beams do not reach the stems, but 
leave a gap.  Chord tremolo beams on half notes are not ambiguous,
as half notes cannot appear in a regular beam, and should reach the 
stems.
";
}
  
\score { 
  \context Voice \notes\relative c {
	\repeat "tremolo" 8 { c16 d16 }
	\repeat "tremolo" 4 { c16 d16 }    
  }
  \paper {
    linewidth=-1.0;
  }  
  \midi { }
}
