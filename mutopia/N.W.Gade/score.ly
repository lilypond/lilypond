\header{
title =         "Imellem Fjeldene. Ouverture";
composer =      "Niels W Gade";
enteredby =     "Mats Bengtsson";
latexheaders = "\\input global";
copyright =	"Mats Bengtsson, 1999. Free circulation permitted and " + 
		"encouraged.\\\\ Typeset from handwritten parts at " +
		"Statens Musikbibliotek, Stockholm, Sweden";
}

\version "1.0.21";

\include "global.ly"
\include "wood.ly"
\include "brass.ly"
\include "strings.ly"
\include "paper16.ly";


\score{ <
  \context StaffGroup = wood <
    \context Voice = flauto <
      \property Staff.instrument = "Flauto"
      \property Staff.instr = "Fl."
      \global
      \marks
      \flauto
    >
    \context Voice = oboe <
      \property Staff.instrument = "Oboe"
      \property Staff.instr = "Ob."
      \global
      \oboe
    >
    \context Voice = clarI <
      \property Staff.instrument = "Clarinetto I"
      \property Staff.instr = "Cl. I"
      \globalNoKey
      \clarI
    >
    \context Voice = clarII <
      \property Staff.instrument = "Clarinetto II"
      \property Staff.instr = "Cl. II"
      \globalNoKey
      \clarII
    >
    \context Voice = fagotto <
      \property Staff.instrument = "Fagotto"
      \property Staff.instr = "Fg."
      \global
      \fagotto
    >
  >
  \context StaffGroup = brass <
    \context Staff = cor <
      \property Staff.instrument = "2 Corni in F"
      \property Staff.instr = "Cor."
      \context Voice = corI <
	\globalNoKey
	\stemup \property Voice.dynamicDir = \up 
	\corI 
      >
      \context Voice = corII { 
	\stemdown \property Voice.dynamicDir = \down 
	\corII 
      }
    >
    \context Staff = trp <
      \property Staff.instrument = "2 Trp. in B\\textflat  "
      \property Staff.instr = "Trp."
      \context Voice = trpI <
	\globalNoKey
	\stemup \property Voice.dynamicDir = \up 
	\trpI
      >
      \context Voice = trpII { 
	\stemdown \property Voice.dynamicDir = \down 
	\trpII
      }
    >
  >
    \context StaffGroup = percussion <\context Voice = timpani <
      \property Staff.instrument = "Timp. \& Triang."
      \property Staff.instr = "Tmp \& Trg"
      \global
      \timpani
    >
  >
  \context StaffGroup = strings <
    \context GrandStaff = violins <
      \context Voice = viI <
        \property Staff.instrument = "Violin I"
        \property Staff.instr = "Vi. I"
        \global
        \viI
      >
      \context Voice = viII <
        \property Staff.instrument = "Violin II"
        \property Staff.instr = "Vi. II"
        \global
        \viII
      >
    >
    \context Voice = vla <
      \property Staff.instrument = "Viola"
      \property Staff.instr = "Vla."
      \global
      \vla
    >
    \context Voice = vlc <
      \property Staff.instrument = "Violoncello"
      \property Staff.instr = "Vlc"
      \global
      \vlc
    >
    \context Voice = cb <
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
        barNumberScriptPadding = "12.0";
    }
    \translator { \StaffContext
	\consists "Staff_margin_engraver";
        marginScriptPadding = "15.0";
	textStyle = "italic";
	textScriptPadding = 5.0;
        textEmptyDimension = 1;
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
