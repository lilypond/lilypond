#(ly:set-option 'old-relative)
\version "2.10.0"
\header {
texidoc = "@cindex Midi Volume Equaliser
The full orchestra plays a notes, where groups stop one after
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
  \set Staff.instrumentName = #"2 Flauti"
  \set Staff.shortInstrumentName = #"Fl."

  c1\f R1*10
}

oboi =  \relative c' {
  \set Staff.midiInstrument = #"oboe"
  \set Staff.instrumentName = #"2 Oboi"
  \set Staff.shortInstrumentName = #"Ob."

  R1*1 c1\f R1*9
}

clarinetti =  \relative c' {
  \set Staff.midiInstrument = #"clarinet"
  \set Staff.instrumentName = #"Clarinetti"
  \set Staff.shortInstrumentName = #"Cl"

  R1*2 c1\f R1*8
}

fagotti =  \relative c' {
  \set Staff.midiInstrument = #"bassoon"
  \set Staff.instrumentName = #"2 Fagotti"
  \set Staff.shortInstrumentName = #"Fg."

  \clef bass
  R1*3 c1\f R1*7
}

corni =  \relative c' {
  \set Staff.midiInstrument = #"french horn"
  \set Staff.instrumentName = #"Corni"
  \set Staff.shortInstrumentName = #"Cor"

  R1*4 c1\f R1*6
}

trombe =  \relative c' {
  \set Staff.midiInstrument = #"trumpet"
  \set Staff.instrumentName = #"Trombe"
  \set Staff.shortInstrumentName = #"Tp."

  \clef bass
  R1*5 c1\f R1*5
}

timpani =  \relative c' {
  \set Staff.midiInstrument = #"timpani"
  \set Staff.instrumentName = #"Timpani"
  \set Staff.shortInstrumentName = #"Timp."

  R1*6 c1\f R1*4
}

violinoI =  \relative c' {
  \set Staff.midiInstrument = #"violin"
  \set Staff.instrumentName = #"Violino I "
  \set Staff.shortInstrumentName = #"Vl. I "

  R1*7 c1\f R1*3
}

violinoII =  \relative c' {
  \set Staff.midiInstrument = #"violin"
  \set Staff.instrumentName = #"Violino II "
  \set Staff.shortInstrumentName = #"Vl. II "
 
  R1*8 c1\f R1*2
}

viola =  \relative c' {
  \set Staff.midiInstrument = #"viola"
  \set Staff.instrumentName = #"Viola"
  \set Staff.shortInstrumentName = #"Vla."

  \clef alto
  R1*9 c1\f R1*1
}

violoncello =  \relative c' {
  \set Staff.midiInstrument = #"cello"
  %\set Staff.midiInstrument = #"contrabass"
  \set Staff.instrumentName = #"Violoncello"
  \set Staff.shortInstrumentName = #"Vc."
  
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
       % Hmm: this forces a staff-bracket, that's good!
       % However, I can't find where is decided on staff-bracket yes/no
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

  \layout {
  	\layoutSixteen
  	indent=100.0\mm
  	line-width=150.0\mm
    \context {
      \RemoveEmptyStaffContext
    }
  }
  
  \midi {
    \context {
      \Score
      tempoWholesPerMinute = #(ly:make-moment 60 1)
      }
    }


}


