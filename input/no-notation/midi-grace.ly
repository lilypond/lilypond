#(ly:set-option 'old-relative)
\version "1.9.4"
\header{ texidoc = "Tests MIDI output with grace notes. " }

\score { 
  \context Voice \notes\relative c {
    \context Voice=VoiceOne
	\grace {
  \property Voice.Stem \override #'stroke-style = #"grace"
   c8 
  \property Voice.Stem \revert #'stroke-style }
 d4 d d d d
	\grace {
  \property Voice.Stem \override #'stroke-style = #"grace"
   e16 f e f 
  \property Voice.Stem \revert #'stroke-style }
 d4 d d d d 
	
  }
  \paper { }  
  \midi { }
}

