\header{
title =         "Imellem Fjeldene. Ouverture";
composer =      "Niels W Gade";
enteredby =     "Mats Bengtsson";
latexheaders =  "\\input global";
copyright =	"Mats Bengtsson, 1999. Free circulation permitted and " + 
		"encouraged.\\\\ Typeset from handwritten parts at " +
		"Statens Musikbibliotek, Stockholm, Sweden";
}

% Process with 'ly2dvi -s parts.ly' to get the separate parts.


\version "1.2.0";

\include "global.ly"
\include "wood.ly"
\include "brass.ly"
\include "strings.ly"

my_paper = \paper {
  textheight = 275.0 \mm;
  \translator {
    \OrchestralPartStaffContext
  }
  \translator { 
    \ScoreContext
    skipBars = 1;
    markScriptPadding = "6.0";
    barNumberScriptPadding = "11.0";
    dynamicPadding = 3.0;
    textScriptPadding = 5.0;
    textStyle = "italic";
    textEmptyDimension = 1;
    noAutoBeaming = "1"; % Beams inserted explicitly as in the original.
  }
}

\score{
  \context Staff <
    \context Voice <
      \global
      \marks
      \flauto
    >
    \context Voice = help \flautohelp
  >
  \header{
    instrument = "Flauto";
  }
  \paper{
    \my_paper
    output = "flauto";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Staff <
    \context Voice <
      \global
      \marks
      \oboe
    >
    \context Voice = help \oboehelp
  >
  \header{
    instrument = "Oboe";
  }
  \paper{
    \my_paper
    output = "oboe";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Voice <
    \globalNoKey
    \marks
    \clarI
  >
  \header{
    instrument = "Clarinetto I in B\\flat";
  }
  \paper{
    \my_paper
    output = "clarI";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Voice <
    \globalNoKey
    \marks
    \clarII
  >
  \header{
    instrument = "Clarinetto II in B\\flat";
  }
  \paper{
    \my_paper
    output = "clarII";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Voice <
    \global
    \marks
    \fagotto
  >
  \header{
    instrument = "Fagotto";
  }
  \paper{
    \my_paper
    output = "fagotto";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Staff <
    \context Voice <
      \globalNoKey
      \marks
      \corI
    >
    \context Voice = help \corIhelp
  >
  \header{
    instrument = "Corno I in F";
  }
  \paper{
    \my_paper
    output = "corI";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Staff <
    \context Voice <
      \globalNoKey
      \marks
      \corII
    >
    \context Voice = help \corIIhelp
  >
  \header{
    instrument = "Corno II in F";
  }
  \paper{
    \my_paper
    output = "corII";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Staff <
    \context Voice <
      \globalNoKey
      \marks
      \trpI
      >
    \context Voice = help \trpIhelp
  >
  \header{
    instrument = "Tromba I in B\\flat";
  }
  \paper{
    \my_paper
    output = "trpI";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Staff <
    \context Voice <
      \globalNoKey
      \marks
      \trpII
    >
    \context Voice = help \trpIIhelp
  >
  \header{
    instrument = "Tromba II in B\\flat";
  }
  \paper{
    \my_paper
    output = "trpII";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Staff <
    \context Voice <
      \globalNoKey
      \marks
      \timpani
    >
    \context Voice = help \timphelp
  >
  \header{
    instrument = "Timpani \& Triangolo";
  }
  \paper{
    \my_paper
    output = "timpani";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Voice <
    \global
    \marks
    \viI
  >
  \header{
    instrument = "Violino I";
  }
  \paper{
    \my_paper
    output = "viI";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Voice <
    \global
    \marks
    \viII
  >
  \header{
    instrument = "Violino II";
  }
  \paper{
    \my_paper
    output = "viII";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Voice <
    \global
    \marks
    \notes{s2.*32 s2*142 \break}
    \vla
  >
  \header{
    instrument = "Viola";
  }
  \paper{
    \my_paper
    output = "viola";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Voice <
    \global
    \marks
    \vlc
  >
  \header{
    instrument = "Violoncello";
  }
  \paper{
    \my_paper
    output = "violoncello";
  }
  \midi {
    \tempo 4=120;
  }
}

\score{
  \context Voice <
    \global
    \marks
    \cb
  >
  \header{
    instrument = "Contrabasso";
  }
  \paper{
    \my_paper
    output = "cb";
  }
  \midi {
    \tempo 4=120;
  }
}
