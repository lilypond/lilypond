flauti = \notes \relative c' {
  \property Staff.instrument	= #"2 Flauti"
  \property Staff.instr		= #"Fl."
  c1 
  \break c
  \bar"|.";
}

oboi = \notes \relative c' {
  \property Staff.instrument	= #"2 Oboi"
  \property Staff.instr		= #"Ob."
  c1 c
}

clarinetti = \notes \relative c' {
  \property Staff.instrument	= #"2 Clarinetti\n(B\\textflat)"
  \property Staff.instr		= #"Cl.\n(B\\textflat)"

  c1 c
}

fagotti = \notes \relative c' {
  \property Staff.instrument	= #"2 Fagotti"
  \property Staff.instr		= #"Fg."
  c1 c
}

corni = \notes \relative c' {
  \property Staff.instrument	= #"2 Corni\n(E\\textflat)"
  \property Staff.instr		= #"Cor.\n(E\\textflat)"

  c1 c
}

trombe = \notes \relative c' {
  \property Staff.instrument	= #"2 Trombe\n(C)"
  \property Staff.instr		= #"Tbe.\n(C)"

  c1 c
}

timpani = \notes \relative c' {
  \property Staff.instrument	= #"Timpani\n(C-G)"
  \property Staff.instr		= #"Timp."

  c1 c
}

violino1 = \notes \relative c' {
  \property Staff.instrument	= #"Violino I"
  \property Staff.instr		= #"Vl. I"
  c1 c
}

violino2 = \notes \relative c' {
  \property Staff.instrument	= #"Violino II"
  \property Staff.instr		= #"Vl. II"
  c1 c
}

viola = \notes \relative c' {
  \property Staff.instrument	= #"Viola"
  \property Staff.instr		= #"Vla."
  c1 c
}

violoncello = \notes \relative c' {
  \property Staff.instrument	= #"Violoncello\ne\nContrabasso"
  \property Staff.instr		= #"Vc.\nCb."
  c1 c
}

\include "paper16.ly"

\score {
  < 
    \context StaffGroup ="legni" < 
      \context Staff ="flauti" \flauti
      \context Staff ="oboi" \oboi
      \context Staff ="clarinetti" \clarinetti 
      \context Staff ="fagotti" \fagotti 
    { %%urg
      \skip 1; 
      \context Staff="flauti" \property Staff.marginScriptPadding = #25
      \context Staff ="oboi" \property Staff.marginScriptPadding = #25
      \context Staff ="clarinetti" \property Staff.marginScriptPadding = #25
      \context Staff ="fagotti" \property Staff.marginScriptPadding = #25
    }
    >
    \context StaffGroup ="ottoni" <
      \context Staff ="corni" \corni
      \context Staff ="trombe" \trombe
    { %%urg
      \skip 1; 
      \context Staff="corni" \property Staff.marginScriptPadding = #25
      \context Staff ="trome" \property Staff.marginScriptPadding = #25
    }
    >
    \context StaffGroup ="timpani" <
      \context Staff ="timpani" \timpani
    { %%urg
      \skip 1; 
      \context Staff="timpani" \property Staff.marginScriptPadding = #25
    }
    >
    \context StaffGroup ="archi" <
      \context GrandStaff ="violini" <
        \context Staff ="violino1" \violino1
        \context Staff ="violino2" \violino2
    { %%urg
      \skip 1; 
      \context Staff="violino1" \property Staff.marginScriptPadding = #25
      \context Staff="violino2" \property Staff.marginScriptPadding = #25
    }
      >
      \context Staff ="viola" \viola
      \context Staff ="violoncello" \violoncello
    { %%urg
      \skip 1; 
      \context Staff="viola" \property Staff.marginScriptPadding = #25
      \context Staff="violoncello" \property Staff.marginScriptPadding = #25
    }
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
      marginScriptPadding = #55  %% urg, this is in pt
    }
  }
}

