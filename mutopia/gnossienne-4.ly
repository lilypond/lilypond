\header {
  filename =    "gnossienne-4.ly";
  title =       "Gnossienne";
  subtitle =	"4";
  source =      "";
  composer =    "Erik Satie (1866-1925)";
  enteredby =   "jcn";
  copyright =   "Public Domain";
}

%{
 Tested Features: cross staff beams and slurs
%}

\version "1.0.14";

\include "nederlands.ly"

global = \notes {
  \key a \minor;
  \time 6/4;
  \cadenza 1;
  \property Staff.timeSignatureStyle = ""
}
  
one = \type Voice=one \notes \relative c'''{
  \clef violin;
  r2 r r r r r 
  r4 [a8--(\< a--] [a-- a-- c-- \!b--] [a--\> fis g \!)e]
}

% upper staff
us = {
  \type Staff=treble
  \skip 1*0;
}

% lower staff
ls = {
  \type Staff=bass
  \skip 1*0;
}

two = \type Voice=two \notes \relative c{
  \stemdown
  \property Staff.slurydirection = 1
% hmm
%  [d,8( a' d f] [a \us d f d] \ls [a f d )a]
  [d,8( a' d f] [a \translator Staff=treble d f d] 
    \translator Staff=bass [a f d )a]
  [d,8( a' d f] [a \translator Staff=treble d f d] 
    \translator Staff=bass [a f d )a]
  [d,8( a' d f] [a \translator Staff=treble d f d] 
    \skip 8*0;
    \translator Staff=bass [a f d )a]
}

\score {
    \type GrandStaff < 
      \type Staff = treble < 
        \global 
	\one
      >
      \type Staff = bass <
        \global
	\clef bass;
        \two
      >
    >

  \paper {
    gourlay_maxmeasures = 4.;
    indent = 8.\mm;
    textheight = 295.\mm;

    % no slur damping
    slur_slope_damping = 10.0;

    %hmm
%    \translator { \BarNumberingScoreContext }
%    \translator { \BarNumberingStaffContext }
%  \translator{ \OrchestralScoreContext }
    \translator{ \OrchestralScoreContext }
    \translator{ 
      \GrandStaffContext
      minVerticalAlign = 3.0*\staffheight;
      maxVerticalAlign = 3.0*\staffheight;
    }
  }
%  \header{
%	  opus = "BWV 847";
%  }

  \midi {
    \tempo 4 = 54;
  }
}

% EOF
