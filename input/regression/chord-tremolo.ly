\version "1.3.146"

\header{
texidoc="
Chord tremolos look like beams, but are a kind of repeat symbol.
To avoid confusion, chord tremolo beams do not reach the stems, but 
leave a gap.  Chord tremolo beams on half notes are not ambiguous,
as half notes cannot appear in a regular beam, and should reach the 
stems.
"
}
  
\score { 
  \context Voice \notes\relative c' {
        % huh -> one beam missing!
	\repeat "tremolo" 8 { d16 e }
	\repeat "tremolo" 4 { d e }
	\repeat "tremolo" 2 { d e }
	\repeat "tremolo" 1 { d e }
	\repeat "tremolo" 1 { d e }
	\break
	\repeat "tremolo" 4 { f'8 e }
	\repeat "tremolo" 2 { f e }    
	\repeat "tremolo" 1 { f e }
  }
  \paper {
    linewidth = 90*\staffspace
  }  
  \midi { }
}
