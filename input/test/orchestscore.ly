\version "1.0.14";

m = \notes \relative c''{
c1 | c2 c | c c | c c | c c | c c | c c | c c | 
}

\score{ <
  \type StaffGroup = wood <
    \type Staff = flauto <
      \property Staff.instrument = "Flauto"
      \property Staff.instr = "Fl."
      \m
    >
    \type Staff = oboe <
      \property Staff.instrument = "Oboe"
      \property Staff.instr = "Ob."
      \m
    >
    \type Staff = clarI <
      \property Staff.instrument = "Clarinetto I"
      \property Staff.instr = "Cl. I"
      \m
    >
    \type Staff = clarII <
      \property Staff.instrument = "Clarinetto II"
      \property Staff.instr = "Cl. II"
      \m
    >
    \type Staff = fagotto <
      \property Staff.instrument = "Fagotto"
      \property Staff.instr = "Fg."
      \m
    >
  >
  \type StaffGroup = brass <
    \type Staff = cor <
      \property Staff.instrument = "2 Corni in F"
      \property Staff.instr = "Cor."
      \type Voice = corI { \stemup \m }
      \type Voice = corII { \stemdown \m }
    >
    \type Staff = trp <
      \property Staff.instrument = "2 Trp. in B\\textflat  "
      \property Staff.instr = "Trp."
      \type Voice = trpI { \stemup \m }
      \type Voice = trpII { \stemdown \m }
    >
  >
    \type StaffGroup = percussion <\type Staff = timpani <
      \property Staff.instrument = "Timpani"
      \property Staff.instr = "Timp."
      \m
    >
  >
  \type StaffGroup = strings <
%    \type GrandStaff = violins <
      \type Staff = viI <
        \property Staff.instrument = "Violin I"
        \property Staff.instr = "Vi. I"
        \m
      >
      \type Staff = viII <
        \property Staff.instrument = "Violin II"
        \property Staff.instr = "Vi. II"
        \m
      >
%    >
    \type Staff = vla <
      \property Staff.instrument = "Viola"
      \property Staff.instr = "Vla."
      \m
    >
    \type Staff = vlc <
      \property Staff.instrument = "Violoncello"
      \property Staff.instr = "Vlc"
      \m
    >
    \type Staff = cb <
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
        minVerticalAlign = 2.5*\staffheight;
    }
    \translator { \StaffContext
	\consists "Staff_margin_engraver";
    }
  }
}

