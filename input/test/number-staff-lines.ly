\header{
texidoc="
The number of stafflines of a staff can be set with the property
numberOfStaffLines.  Ledger lines both on note heads and rests are
adjusted.  Barlines also are adjusted.
";
}

\score { 
  \context Voice \notes\relative c {
    
	c' c c c | g' g g g \property Staff . numberOfStaffLines = 3
	
  }
  \paper { }  
  \midi { }
}
