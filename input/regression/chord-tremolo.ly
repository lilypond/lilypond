\version "1.5.68"

\header{
texidoc="
Chord tremolos look like beams, but are a kind of repeat symbol.
To avoid confusion, chord tremolo beams do not reach the stems, but 
leave a gap.  Chord tremolo beams on half notes are not ambiguous,
as half notes cannot appear in a regular beam, and should reach the 
stems.

(To ensure that the spacing engine is not confused we add some regular notes as well.) 
"
}
  
\score { 
  \context Voice \notes\relative c' {
        % huh -> one beam missing!
	\repeat "tremolo" 8 { d16 e }
	\repeat "tremolo" 4 { d e }
	\repeat "tremolo" 2 { d e }
	c4
	\break
	\repeat "tremolo" 4 { f'8 e }
	\repeat "tremolo" 2 { f e }
	c4
	c4 c4 c4 c4
	c4 c4 c4 c4
	c4 c4 c4 c4

  }
  \paper {
    linewidth = 90*\staffspace
  }  
  \midi { }
}
