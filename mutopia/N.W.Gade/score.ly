\header{
title =         "Imellem Fjeldene. Ouverture";
composer =      "Niels W Gade";
enteredby =     "Mats Bengtsson";
latexheaders = "\\input global";
copyright =	"Mats Bengtsson, 1999. Free circulation permitted and " + 
		"encouraged.\\\\ Typeset from handwritten parts at " +
		"Statens Musikbibliotek, Stockholm, Sweden";
}

\version "1.0.20";

\include "global.ly"
\include "wood.ly"
\include "brass.ly"
\include "strings.ly"
\include "paper16.ly";


\score{ <
  \context StaffGroup = wood <
    \context Staff = flauto <
      \property Staff.instrument = "Flauto"
      \property Staff.instr = "Fl."
      \global
      \marks
      \flauto
    >
    \context Staff = oboe <
      \property Staff.instrument = "Oboe"
      \property Staff.instr = "Ob."
      \global
      \oboe
    >
    \context Staff = clarI <
      \property Staff.instrument = "Clarinetto I"
      \property Staff.instr = "Cl. I"
      \globalNoKey
      \clarI
    >
    \context Staff = clarII <
      \property Staff.instrument = "Clarinetto II"
      \property Staff.instr = "Cl. II"
      \globalNoKey
      \clarII
    >
    \context Staff = fagotto <
      \property Staff.instrument = "Fagotto"
      \property Staff.instr = "Fg."
      \global
      \fagotto
    >
  >
  \context StaffGroup = brass <
    \context Staff = cor <
      \globalNoKey
      \property Staff.instrument = "2 Corni in F"
      \property Staff.instr = "Cor."
      \context Voice = corI { \stemup \corI }
      \context Voice = corII { \stemdown \corII }
    >
    \context Staff = trp <
      \globalNoKey
      \property Staff.instrument = "2 Trp. in B\\textflat  "
      \property Staff.instr = "Trp."
      \context Voice = trpI { \stemup \trpI }
      \context Voice = trpII { \stemdown \trpII }
    >
  >
    \context StaffGroup = percussion <\context Staff = timpani <
      \property Staff.instrument = "Timp. \& Triang."
      \property Staff.instr = "Timp. \& Triang."
      \global
      \timpani
    >
  >
  \context StaffGroup = strings <
    \context GrandStaff = violins <
      \context Staff = viI <
        \property Staff.instrument = "Violin I"
        \property Staff.instr = "Vi. I"
        \global
        \viI
      >
      \context Staff = viII <
        \property Staff.instrument = "Violin II"
        \property Staff.instr = "Vi. II"
        \global
        \viII
      >
    >
    \context Staff = vla <
      \property Staff.instrument = "Viola"
      \property Staff.instr = "Vla."
      \global
      \vla
    >
    \context Staff = vlc <
      \property Staff.instrument = "Violoncello"
      \property Staff.instr = "Vlc"
      \global
      \vlc
    >
    \context Staff = cb <
      \property Staff.instrument = "Contrabasso"
      \property Staff.instr = "C.B."
      \global
      \cb
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
        marginScriptPadding = "15.0";
	textstyle = "italic";
	textScriptPadding = 5.0;
        textEmptyDimension = 1;
%        oldTieBehavior = 1;
    }
    \translator { \VoiceContext
        oldTieBehavior = 1;
    }
  }
}

\score{
  \context StaffGroup <
    \context Staff = oboe \oboe
    \context Staff = flauto \flauto
    \context Staff = clarinetsInBes {\notes \transpose bes <\clarI \clarII >}
    \context Staff = fagotto \fagotto
    \context Staff = corniInF {\notes \transpose f <\corI \corII >}
    \context Staff = trumpetsInBes {\notes \transpose bes <\trpI \trpII >}
    \context Staff = timpani \timpani
    \context Staff = violinoi \viI
    \context Staff = violinoii \viII
    \context Staff = viola \vla
    \context Staff = violoncello \vlc
    \context Staff = contrabass \cb
  >
  \midi {
    \tempo 4=120;
  }
}
