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
%  \cadenza 1;
  \skip 1.*34;
  \bar ".|";
}
  
upper = \type Voice=one \notes \relative c''{
  \clef violin;
  \stemup
  r2 r r 
  r2 r r 
  r4 [a'8--(\< a--] [a-- a-- c-- \!b--] [a--\> fis g \!e]
  % grace hack
  [es8 { \type Voice=urgnobeam \tiny b8*1/16 ~ \normalsize } )c*15/16] r4 r2 r
  r2 r r
  r4 [a'8--(\< a--] [a-- a-- c-- \!b--] [a--\> fis g \!e]
  [es8 { \type Voice=urgnobeam \tiny b8*1/16 ~ \normalsize } )c*15/16] r4 r2 r
  r4 [g16( a bes a] [g a bes a g a bes a] [g a bes a g fis es fis] 
  )d4 \tiny fis8*1/16 ~ \normalsize gis4*31/32 ~ gis8 r r4 r2
  r4 [g16( a bes a] [g a bes a g a bes a] [g a bes a g fis es fis] 
  )d4 \tiny fis8*1/16 ~ \normalsize gis4*31/32 ~ gis8 r r4 r2
  \tiny a8*1/16 ~ \normalsize f4*31/32 ~ f8 r r2 r
  r2 r4 [a8( b][c d c b] \tiny b8*1/16 ~ \normalsize [e8*15/16 
  { \type Voice=urgnobeam \tiny a,8*1/16 ~ \normalsize } )g8*15/16] r4 r2 r
  r2 r4 [a8( b][c d c b] [a b c d][c b a b][c d c b]
  \tiny b8*1/16 ~ \normalsize [e8*15/16 
  { \type Voice=urgnobeam \tiny a,8*1/16 ~ \normalsize } )g8*15/16] r4 r2 r
  a2( \tiny e'8*1/16 ~ \normalsize f4*31/32 ~ )f8 r r2
  r2 r r
  fis,4( \tiny dis8*1/16 \normalsize <)cis4*31/32 ais> r2 r
  \tiny b'8*1/16 ~ \normalsize [a8*15/16 
  { \type Voice=urgnobeam \tiny b8*1/16 ~ \normalsize } a8*15/16] r4 r2 r
  r4 [a'8--(\< a--] [a-- a-- c-- \!b--] [a--\> fis g \!e]
  [es8 { \type Voice=urgnobeam \tiny b8*1/16 ~ \normalsize } )c*15/16] r4 r2 r
  d,4( \tiny fis8*1/16 ~ \normalsize gis4*31/32 ~ )gis8 r r4 r2
  f4 ~ f8 r r2 r
  [f'8( g a b][a g f g][a b a g]
  \tiny f8*1/16 ~ \normalsize [g8*15/16 
  { \type Voice=urgnobeam \tiny d8*1/16 ~ \normalsize } )e8*15/16] r4 r2 r
  [f8( g a b][a g f g][a b a g]
  \tiny f8*1/16 ~ \normalsize [g8*15/16 
  { \type Voice=urgnobeam \tiny d8*1/16 ~ \normalsize } )e8*15/16] r4 r2 r
  a,2( \tiny e'8*1/16 ~ \normalsize f4*31/32 ~ )f8 r r2
  r2 r r
  fis,4( \tiny dis8*1/16 \normalsize <)cis4*31/32 ais> r2 r
  <e1 g b e> ~ <e g b e>
}

basloopje = \notes\relative c{
  d,8( a' d f a d f d a f d )a
}

bassbeam = \notes{
  [s2] [s8 \translator Staff=treble s s s] [\translator Staff=bass s2]
%   [s2] [s2] [s2]
}

lower = \type Voice=two \notes \relative c{
  \stemdown
  \property Staff.slurydirection = 1

  % snapnie, hoevaak relative c heeft ze nodig?
  < \notes\relative c \basloopje \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \transpose bes \notes\relative c{ \basloopje } \bassbeam >
  < \transpose bes \notes\relative c{ \basloopje } \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \transpose bes \notes\relative c{ \basloopje } \bassbeam >
  < \transpose bes \notes\relative c{ \basloopje } \bassbeam >
  < \transpose a \notes\relative c{ \basloopje } \bassbeam >
  < \transpose bes \notes\relative c{ \basloopje } \bassbeam >
  < \transpose a \notes\relative c{ \basloopje } \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  % huh? d'
  < \transpose d' \notes\relative c{ \basloopje } \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \transpose d' \notes\relative c{ \basloopje } \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \transpose e' \notes\relative c{ \basloopje } \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \transpose bes \notes\relative c{ \basloopje } \bassbeam >
  < \transpose a \notes\relative c{ \basloopje } \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \transpose d' \notes\relative c{ \basloopje } \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \transpose d' \notes\relative c{ \basloopje } \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \notes\relative c \basloopje \bassbeam >
  < \transpose e' \notes\relative c{ \basloopje } \bassbeam >
  < e1 b' e> ~ < e b' e> 
}

\score {
    \type GrandStaff < 
      \type Staff = treble < 
        \global 
	\upper
      >
      \type Staff = bass <
        \global
	\clef bass;
        \lower
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
      % don't display bars?
      barAlways = 0.;
    }
    \translator{ 
      \StaffContext
% hmm, i don't want bars
% but i do want the staffs to be connected
%      \remove "Bar_engraver";
      \remove "Time_signature_engraver";
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
