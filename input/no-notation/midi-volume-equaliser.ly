#(ly:set-option 'old-relative)
\version "1.9.0"
\header {
texidoc = "@cindex Midi Volume Equaliser
The full orchestra plays a notes, where groups stop one after
another. Use this to tune equalizer settings. "
}

\include "paper16-init.ly"

%{

Override, see scm/midi.scm:

#(set! instrument-equalizer-alist
      (append 
       '(
	 ("flute" . (0 . 0.7))
        )
      instrument-equalizer-alist))

%}

flauti = \notes \relative c' {
  \property Staff.midiInstrument = #"flute"
  \property Staff.instrument	= #"2 Flauti"
  \property Staff.instr		= #"Fl."

  c1-\f R1*10
}

oboi = \notes \relative c' {
  \property Staff.midiInstrument = #"oboe"
  \property Staff.instrument	= #"2 Oboi"
  \property Staff.instr		= #"Ob."

  R1*1 c1-\f R1*9
}

clarinetti = \notes \relative c' {
  \property Staff.midiInstrument = #"clarinet"
  \property Staff.instrument	= #"Clarinetti"
  \property Staff.instr		= #"Cl"

  R1*2 c1-\f R1*8
}

fagotti = \notes \relative c' {
  \property Staff.midiInstrument = #"bassoon"
  \property Staff.instrument	= #"2 Fagotti"
  \property Staff.instr		= #"Fg."

  \clef bass
  R1*3 c1-\f R1*7
}

corni = \notes \relative c' {
  \property Staff.midiInstrument = #"french horn"
  \property Staff.instrument	= #"Corni"
  \property Staff.instr		= #"Cor"

  R1*4 c1-\f R1*6
}

trombe = \notes \relative c' {
  \property Staff.midiInstrument = #"trumpet"
  \property Staff.instrument	= #"Trombe"
  \property Staff.instr		= #"Tp."

  \clef bass
  R1*5 c1-\f R1*5
}

timpani = \notes \relative c' {
  \property Staff.midiInstrument = #"timpani"
  \property Staff.instrument	= #"Timpani"
  \property Staff.instr		= #"Timp."

  R1*6 c1-\f R1*4
}

violinoI = \notes \relative c' {
  \property Staff.midiInstrument = #"violin"
  \property Staff.instrument	= #"Violino I "
  \property Staff.instr		= #"Vl. I "

  R1*7 c1-\f R1*3
}

violinoII = \notes \relative c' {
  \property Staff.midiInstrument = #"violin"
  \property Staff.instrument	= #"Violino II "
  \property Staff.instr		= #"Vl. II "
 
  R1*8 c1-\f R1*2
}

viola = \notes \relative c' {
  \property Staff.midiInstrument = #"viola"
  \property Staff.instrument	= #"Viola"
  \property Staff.instr		= #"Vla."

  \clef alto
  R1*9 c1-\f R1*1
}

violoncello = \notes \relative c' {
  \property Staff.midiInstrument = #"cello"
  %\property Staff.midiInstrument = #"contrabass"
  \property Staff.instrument	= #"Violoncello"
  \property Staff.instr		= #"Vc."
  
  \clef bass
  R1*10 c1-\f
}


\score {
  < 
    \context StaffGroup ="legni" < 
      \context Staff ="flauti" \flauti
      \context Staff ="oboi" \oboi
      \context Staff ="clarinetti" \clarinetti 
      \context Staff ="fagotti" \fagotti 
    >
    \context StaffGroup ="ottoni" <
      \context Staff ="corni" \corni
      \context Staff ="trombe" \trombe
    >
    \context StaffGroup ="timpani" <
      \context Staff ="timpani" \timpani
     { 
       \skip 1 
       % Hmm: this forces a staff-bracket, that's good!
       % However, I can't find where is decided on staff-bracket yes/no
     }
    >
    \context StaffGroup ="archi" <
      \context GrandStaff ="violini" <
        \context Staff ="violino1" \violinoI
        \context Staff ="violino2" \violinoII
      >
      \context Staff ="viola" \viola
      \context Staff ="violoncello" \violoncello
    >
  >

  \paper {
  	\paperSixteen
  	indent=100.0\mm
  	linewidth=150.0\mm
    \translator {
      \RemoveEmptyStaffContext
    }
  }
  \midi {
  	\tempo 1 = 60
  }
}


