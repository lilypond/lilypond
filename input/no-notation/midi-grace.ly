#(ly:set-option 'old-relative)
\version "1.9.1"
\header{ texidoc = "Tests MIDI output with grace notes. " }

\score { 
  \context Voice \notes\relative c {
    \context Voice=VoiceOne
	\grace c8 d4 d d d d
	\grace { e16 f e f } d4 d d d d 
	
  }
  \paper { }  
  \midi { }
}

