
\header{
%% URG
Clarinetti ="\\vbox to0pt{\vss\\hbox to0pt{\\hss2 Clarinetti\\hss}\\hbox to0pt{\\hss (B\\textflat)\\hss}\vss}";
Cl ="\\vbox to0pt{\vss\\hbox to0pt{\\hss Cl.\\hss}\\hbox to0pt{\\hss (B\\textflat)\\hss}\vss}";
Corni ="\\vbox to0pt{\vss\\hbox to0pt{\\hss2 Corni\\hss}\\hbox to0pt{\\hss (E\\textflat)\\hss}\vss}";
Cor ="\\vbox to0pt{\vss\\hbox to0pt{\\hss Cor.\\hss}\\hbox to0pt{\\hss (E\\textflat)\\hss}\vss}";
Trombe ="\\vbox to0pt{\vss\\hbox to0pt{\\hss2 Trombe\\hss}\\hbox to0pt{\\hss (C)\\hss}\vss}";
Tbe ="\\vbox to0pt{\vss\\hbox to0pt{\\hss Tbe.\\hss}\\hbox to0pt{\\hss (C)\\hss}\vss}";
Timpani ="\\vbox to0pt{\vss\\hbox to0pt{\\hss Timpani\\hss}\\hbox to0pt{\\hss (C-G)\\hss}\vss}";
VioloncelloContrabasso ="\\hss\\vbox to0pt{\vss\\hbox to0pt{\\hss Violoncello\\hss}\\hbox to0pt{\\hss e\\hss}\\hbox to0pt{\\hss Contrabasso\\hss}\vss}";
VcCb ="\\hss\\vbox to0pt{\vss\\hbox to0pt{\\hss Vc.\\hss}\\hbox to0pt{\\hss Cb.\\hss}\vss}";
}

flauti = \notes \relative c' {
  \property Staff.instrument	= #"2 Flauti"
  \property Staff.instr		= #"Fl."
  c1 \break c
  \bar"|.";
}

oboi = \notes \relative c' {
  \property Staff.instrument	= #"2 Oboi"
  \property Staff.instr		= #"Ob."
  c1 c
}

clarinetti = \notes \relative c' {
  %%\property Staff.instrument	= #"\\mudelaClarinetti         "
  %%\property Staff.instr	= #"\\mudelaCl    "

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
  %%\property Staff.instrument	= #"\\mudelaCorni      "
  %%\property Staff.instr	= #"\\mudelaCor    "

  \property Staff.instrument	= #"2 Corni\n(E\\textflat)"
  \property Staff.instr		= #"Cor.\n(E\\textflat)"

  c1 c
}

trombe = \notes \relative c' {
  %%\property Staff.instrument	= #"\\mudelaTrombe        "
  %%\property Staff.instr	= #"\\mudelaTbe    "

  \property Staff.instrument	= #"2 Trombe\n(C)"
  \property Staff.instr		= #"Tbe.\n(C)"

  c1 c
}

timpani = \notes \relative c' {
  %%\property Staff.instrument	= #"\\mudelaTimpani         "
  %%\property Staff.instr	= #"Timp."

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
  %%\property Staff.instrument	= #"\\mudelaVioloncelloContrabasso         "
  %%\property Staff.instr	= #"\\mudelaVcCb    "

  \property Staff.instrument	= #"Violoncello\ne\nContrabasso"
  \property Staff.instr		= #"Vc.\nCb."
  c1 c
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
  	indent=100.0\mm;
  	linewidth=150.0\mm;
    \translator {
      \StaffContext
      \consists Staff_margin_engraver;
      staffMarginHorizontalPadding = #-12
    }
  }
}

