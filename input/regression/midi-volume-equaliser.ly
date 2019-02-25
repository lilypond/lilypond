\version "2.21.0"
\header {
  texidoc = "@cindex Midi Volume Equaliser
The full orchestra plays a note, where groups stop one after
another. Use this to tune equalizer settings. "
}

#(set-global-staff-size 16)

%{

Override, see scm/midi.scm:

#(set! instrument-equalizer-alist
  (append 
   '(
     ("flute" . (0 . 0.7))
   )
   instrument-equalizer-alist))

%}

flauti =  \relative {
  \set Staff.midiInstrument = "flute"

  c'1\f R1*10
}

oboi =  \relative {
  \set Staff.midiInstrument = "oboe"

  R1*1 c'1\f R1*9
}

clarinetti =  \relative {
  \set Staff.midiInstrument = "clarinet"

  R1*2 c'1\f R1*8
}

fagotti =  \relative {
  \set Staff.midiInstrument = "bassoon"

  \clef bass
  R1*3 c'1\f R1*7
}

corni =  \relative {
  \set Staff.midiInstrument = "french horn"

  R1*4 c'1\f R1*6
}

trombe =  \relative {
  \set Staff.midiInstrument = "trumpet"

  \clef bass
  R1*5 c'1\f R1*5
}

timpani =  \relative {
  \set Staff.midiInstrument = "timpani"

  R1*6 c'1\f R1*4
}

violinoI =  \relative {
  \set Staff.midiInstrument = "violin"

  R1*7 c'1\f R1*3
}

violinoII =  \relative {
  \set Staff.midiInstrument = "violin"
  
  R1*8 c'1\f R1*2
}

viola =  \relative {
  \set Staff.midiInstrument = "viola"

  \clef alto
  R1*9 c'1\f R1*1
}

violoncello =  \relative {
  \set Staff.midiInstrument = "cello"
				%\set Staff.midiInstrument = "contrabass"
  
  \clef bass
  R1*10 c'1\f
}



\score {
  << 
    \new StaffGroup = "legni" << 
      \new Staff = "flauti" \flauti
      \new Staff = "oboi" \oboi
      \new Staff = "clarinetti" \clarinetti 
      \new Staff = "fagotti" \fagotti 
    >>
    \new StaffGroup = "ottoni" <<
      \new Staff = "corni" \corni
      \new Staff = "trombe" \trombe
    >>
    \new StaffGroup = "timpani" <<
      \new Staff = "timpani" \timpani
      { 
	\skip 1 
	%% Hmm: this forces a staff-bracket, that's good!
	%% However, I can't find where is decided on staff-bracket yes/no
      }
    >>
    \new StaffGroup = "archi" <<
      \new GrandStaff = "violini" <<
        \new Staff = "violino1" \violinoI
        \new Staff = "violino2" \violinoII
      >>
      \new Staff = "viola" \viola
      \new Staff = "violoncello" \violoncello
    >>
  >>

  \midi {
    \tempo 1 = 60
  }
}


