\version "1.3.59";

m = \notes \relative c''{

c1 | c2 c | c c | c c | \break c c | c c | c c | c c | 
}

\score{ < 
  \context StaffGroup = wood <
    \context Staff = flauto <
      \property Staff.instrument = "Flauto"
      \property Staff.instr = "Fl."
      \m
    >
    \context Staff = oboe <
      \property Staff.instrument = "Oboe"
      \property Staff.instr = "Ob."
      \m
    >
    \context Staff = clarI <
      \property Staff.instrument = "Clarinetto I"
      \property Staff.instr = "Cl. I"
      \m
    >
    \context Staff = clarII <
      \property Staff.instrument = "Clarinetto II"
      \property Staff.instr = "Cl. II"
      \m
    >
    \context Staff = fagotto <
      \property Staff.instrument = "Fagotto"
      \property Staff.instr = "Fg."
      \m
    >
  >
  \context StaffGroup = brass <
    \context Staff = cor <
      \property Staff.instrument = "2 Corni in F"
      \property Staff.instr = "Cor."
      \context Voice = corI { \stemup \m }
      \context Voice = corII { \stemdown \m }
    >
    \context Staff = trp <
      \property Staff.instrument = "2 Trp. in B\\textflat  "
      \property Staff.instr = "Trp."
      \context Voice = trpI { \stemup \m }
      \context Voice = trpII { \stemdown \m }
    >
  >
    \context StaffGroup = percussion <\context Staff = timpani <
      \property Staff.instrument = "Timpani"
      \property Staff.instr = "Timp."
      \notes{c''1 R1*8}
    >
  >
  \context StaffGroup = strings <
    \context GrandStaff = violins <
      \context Staff = viI <
        \property Staff.instrument = "Violin I"
        \property Staff.instr = "Vi. I"
        \m
      >
      \context Staff = viII <
        \property Staff.instrument = "Violin II"
        \property Staff.instr = "Vi. II"
        \m
      >
    >
    \context Staff = vla <
      \property Staff.instrument = "Viola"
      \property Staff.instr = "Vla."
      \m
    >
    \context Staff = vlc <
      \property Staff.instrument = "Violoncello"
      \property Staff.instr = "Vlc"
      \m
    >
    \context Staff = cb <
      \property Staff.instrument = "Contrabasso"
      \property Staff.instr = "C.B."
      \m
    >
  >
>
 \paper {
%    \paper_sixteen;
    linewidth = 185.\mm;
    textheight = 260.\mm;
    \translator {
	\OrchestralScoreContext
	barNumberScriptPadding = 10;
        minVerticalAlign = 2.2*\staffheight;

    }
    \translator { \HaraKiriStaffContext
	\consists "Instrument_name_engraver";
        marginScriptPadding = 15.0;
    }
  }
}

