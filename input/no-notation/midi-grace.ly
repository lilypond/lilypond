#(ly:set-option 'old-relative)
\version "2.2.0"
\header{ texidoc = "Tests MIDI output with grace notes. " }

\score { 
  \context Voice \notes\relative c {
    \context Voice=VoiceOne
	\grace {
  \override Stem  #'stroke-style = #"grace"
   c8 
  \revert Stem #'stroke-style }
 d4 d d d d
	\grace {
  \override Stem  #'stroke-style = #"grace"
   e16 f e f 
  \revert Stem #'stroke-style }
 d4 d d d d 
	
  }
  \paper { }  
  \midi { }
}

