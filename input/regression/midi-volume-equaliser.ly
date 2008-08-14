\version "2.11.51"
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

flauti =  \relative c' {
  \set Staff.midiInstrument = #"flute"

  c1\f R1*10
}

oboi =  \relative c' {
  \set Staff.midiInstrument = #"oboe"

  R1*1 c1\f R1*9
}

clarinetti =  \relative c' {
  \set Staff.midiInstrument = #"clarinet"

  R1*2 c1\f R1*8
}

fagotti =  \relative c' {
  \set Staff.midiInstrument = #"bassoon"

  \clef bass
  R1*3 c1\f R1*7
}

corni =  \relative c' {
  \set Staff.midiInstrument = #"french horn"

  R1*4 c1\f R1*6
}

trombe =  \relative c' {
  \set Staff.midiInstrument = #"trumpet"

  \clef bass
  R1*5 c1\f R1*5
}

timpani =  \relative c' {
  \set Staff.midiInstrument = #"timpani"

  R1*6 c1\f R1*4
}

violinoI =  \relative c' {
  \set Staff.midiInstrument = #"violin"

  R1*7 c1\f R1*3
}

violinoII =  \relative c' {
  \set Staff.midiInstrument = #"violin"
  
  R1*8 c1\f R1*2
}

viola =  \relative c' {
  \set Staff.midiInstrument = #"viola"

  \clef alto
  R1*9 c1\f R1*1
}

violoncello =  \relative c' {
  \set Staff.midiInstrument = #"cello"
				%\set Staff.midiInstrument = #"contrabass"
  
  \clef bass
  R1*10 c1\f
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
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 60 1)
    }
  }
}


