
flauti = \notes \relative c' {
  \property Staff.midiInstrument = #"flute"
  \property Staff.instrument	= #"2 Flauti"
  \property Staff.instr		= #"Fl."

  c1 R1*10
}

oboi = \notes \relative c' {
  \property Staff.midiInstrument = #"oboe"
  \property Staff.instrument	= #"2 Oboi"
  \property Staff.instr		= #"Ob."

  R1*1 c1 R1*9
}

clarinetti = \notes \relative c' {
  \property Staff.midiInstrument = #"clarinet"
  \property Staff.instrument	= #"2 Clarinetti\n(B\\textflat)"
  \property Staff.instr		= #"Cl.\n(B\\textflat)"

  R1*2 c1 R1*8
}

fagotti = \notes \relative c' {
  \property Staff.midiInstrument = #"bassoon"
  \property Staff.instrument	= #"2 Fagotti"
  \property Staff.instr		= #"Fg."

  R1*3 c1 R1*7
}

corni = \notes \relative c' {
  \property Staff.midiInstrument = #"french horn"
  \property Staff.instrument	= #"2 Corni\n(E\\textflat)"
  \property Staff.instr		= #"Cor.\n(E\\textflat)"

  R1*4 c1 R1*6
}

trombe = \notes \relative c' {
  \property Staff.midiInstrument = #"trumpet"
  \property Staff.instrument	= #"2 Trombe\n(C)"
  \property Staff.instr		= #"Tbe.\n(C)"

  R1*5 c1 R1*5
}

timpani = \notes \relative c' {
  \property Staff.midiInstrument = #"timpani"
  \property Staff.instrument	= #"Timpani\n(C-G)"
  \property Staff.instr		= #"Timp."

  R1*6 c1 R1*4
}

violino1 = \notes \relative c' {
  \property Staff.midiInstrument = #"violin"
  \property Staff.instrument	= #"Violino I"
  \property Staff.instr		= #"Vl. I"

  R1*7 c1 R1*3
}

violino2 = \notes \relative c' {
  \property Staff.midiInstrument = #"violin"
  \property Staff.instrument	= #"Violino II"
  \property Staff.instr		= #"Vl. II"
 
  R1*8 c1 R1*2
}

viola = \notes \relative c' {
  \property Staff.midiInstrument = #"viola"
  \property Staff.instrument	= #"Viola"
  \property Staff.instr		= #"Vla."

  R1*9 c1 R1*1
}

violoncello = \notes \relative c' {
  \property Staff.midiInstrument = #"cello"
  %\property Staff.midiInstrument = #"contrabass"
  \property Staff.instrument	= #"Violoncello\ne\nContrabasso"
  \property Staff.instr		= #"Vc.\nCb."
  
  R1*10 c1
}

\include "paper16.ly"

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
       \skip 1; 
       % Hmm: this forces a staff-bracket, that's good!
       % However, I can't find where is decided on staff-bracket yes/no
     }
    >
    \context StaffGroup ="archi" <
      \context GrandStaff ="violini" <
        \context Staff ="violino1" \violino1
        \context Staff ="violino2" \violino2
      >
      \context Staff ="viola" \viola
      \context Staff ="violoncello" \violoncello
    >
  >
 \header{
		title = "Coriolan";
		subtitle = "Ouverture"; 
		opus = "Opus 62";
		composer = "Ludwig van Beethoven (1770-1827)";
		enteredby = "JCN";
		copyright = "public domain";
	}

  \paper {
  	\paper_sixteen
  	indent=100.0\mm;
  	linewidth=150.0\mm;
    \translator {
      \HaraKiriStaffContext
    }
  }
  \midi {
  	\tempo 1 = 60;
  }
}

