\header{
title =         "Imellan Fjeldene. Ouverture";
composer =      "Niels W Gade";
enteredby =     "Mats Bengtsson";
latexheaders = "\\input global";
copyright =	"Mats Bengtsson, 1999. Free circulation permitted and " + 
		"encouraged.\\\\ Typeset from handwritten parts at " +
		"Statens Musikbibliotek, Stockholm, Sweden";
}

\version "1.0.14";

\include "global.ly"
\include "wood.ly"
\include "brass.ly"
\include "strings.ly"
\include "paper16.ly";


\score{ <
  \type StaffGroup = wood <
    \type Staff = flauto <
      \property Staff.instrument = "Flauto"
      \property Staff.instr = "Fl."
      \global
      \flauto
    >
    \type Staff = oboe <
      \property Staff.instrument = "Oboe"
      \property Staff.instr = "Ob."
      \global
      \marks
      \oboe
    >
    \type Staff = clarI <
      \property Staff.instrument = "Clarinetto I"
      \property Staff.instr = "Cl. I"
      \global
      \clarI
    >
    \type Staff = clarII <
      \property Staff.instrument = "Clarinetto II"
      \property Staff.instr = "Cl. II"
      \global
      \clarII
    >
    \type Staff = fagotto <
      \property Staff.instrument = "Fagotto"
      \property Staff.instr = "Fg."
      \global
      \fagotto
    >
  >
  \type StaffGroup = brass <
    \type Staff = cor <
      \global
      \property Staff.instrument = "2 Corni in F"
      \property Staff.instr = "Cor."
      \type Voice = corI { \stemup \corI }
      \type Voice = corII { \stemdown \corII }
    >
    \type Staff = trp <
      \global
      \property Staff.instrument = "2 Trp. in B\\textflat  "
      \property Staff.instr = "Trp."
      \type Voice = trpI { \stemup \trpI }
      \type Voice = trpII { \stemdown \trpII }
    >
  >
    \type StaffGroup = percussion <\type Staff = timpani <
      \property Staff.instrument = "Timpani"
      \property Staff.instr = "Timp."
      \global
      \timpani
    >
  >
  \type StaffGroup = strings <
    \type GrandStaff = violins <
      \type Staff = viI <
        \property Staff.instrument = "Violin I"
        \property Staff.instr = "Vi. I"
        \global
        \viI
      >
      \type Staff = viII <
        \property Staff.instrument = "Violin II"
        \property Staff.instr = "Vi. II"
        \global
        \viII
      >
    >
    \type Staff = vla <
      \property Staff.instrument = "Viola"
      \property Staff.instr = "Vla."
      \global
      \vla
    >
    \type Staff = vlc <
      \property Staff.instrument = "Violoncello"
      \property Staff.instr = "Vlc"
      \global
      \vlc
    >
    \type Staff = cb <
      \property Staff.instrument = "Contrabasso"
      \property Staff.instr = "C.B."
      \global
      \cb
    >
  >
>
 \paper {
    \paper_sixteen
    linewidth = 185.\mm;
    textheight = 260.\mm;
    \translator {
	\OrchestralScoreContext
        minVerticalAlign = 2.5*\staffheight;
    }
    \translator { \StaffContext
	\consists "Staff_margin_engraver";
        marginHangOnClef = 1;
        marginScriptPadding = "20.0";
	textstyle = "italic";
	textScriptPadding = 5.0;
    }
  }
}

\score{
  \type StaffGroup <
    \type Staff = oboe \oboe
    \type Staff = flauto \flauto
    \type Staff = clarinetsInBes {\notes \transpose bes <\clarI \clarII >}
    \type Staff = fagotto \fagotto
    \type Staff = corniInF {\notes \transpose f <\corI \corII >}
    \type Staff = trumpetsInBes {\notes \transpose bes <\trpI \trpII >}
    \type Staff = timpani \timpani
    \type Staff = violinoi \viI
    \type Staff = violinoii \viII
    \type Staff = viola \vla
    \type Staff = violoncello \vlc
    \type Staff = contrabass \cb
  >
  \midi {
    \tempo 4=120;
  }
}
