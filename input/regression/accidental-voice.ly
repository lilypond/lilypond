\version "2.2.0"

\header{
texidoc="
This shows how modern cross voice auto cautionary accidentals are handled.
The first two fisses get accidentals because they belong to different voices.
The first f gets cautionary natural because of previous measure.
The last f gets cautionary natural because fis was only in the other voice.
"
}



voicea = \notes \transpose c c' {
    \stemUp
    fis2 a2 f4 fis a2
}
voiceb = \notes \transpose c c' {
    \stemDown
    c2 fis2  f4 c   f2
}

\score {
    <<
	\notes
	\new NoteNames {
	    \set printOctaveNames = ##f
	     \voicea
	     }
	\context Staff << 
	    #(set-accidental-style 'modern-voice-cautionary)
	    \new Voice \voicea
	    \new Voice \voiceb
	>>
	\new NoteNames {
	    \set printOctaveNames = ##f
	    \voiceb

			 }
    >>
\paper { raggedright = ##t }
}
