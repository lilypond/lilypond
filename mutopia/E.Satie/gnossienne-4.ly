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
 Tested Features: cross staff beams and slurs, grace notes, no bars
%}

\version "1.0.21";

\include "nederlands.ly"

global = \notes {
  \key a \minor;
  \time 6/4;
%  \cadenza 1;
  \skip 1.*34;
  \bar ".|";
}
  
upper = \context Staff=treble \notes\relative c''{
  \clef violin;
  \stemup
  \context Voice=one
  r2 r r 
  r2 r r
  r4 [a'8--(\< a--] [a-- a-- c-- \!b--] [a--\> gis f \!e]
  % grace hack
  < { [es8 )c] } \context Voice=x { \stemup s8*1/2 \tiny b8*1/2 ~ } > r4 r2 r
  r2 r r
  r4 [a'8--(\< a--] [a-- a-- c-- \!b--] [a--\> gis f \!e]
  < { [es8 )c] } \context Voice=x { \stemup s8*1/2 \tiny b8*1/2 ~ } > r4 r2 r
  r4 [g16( a bes a] [g a bes a g a bes a] [g a bes a g fis es fis] 
  )d4 \tiny fis8*1/2 ~ \normalsize gis4*3/4 ~ gis8 r r4 r2
  r4 [g16( a bes a] [g a bes a g a bes a] [g a bes a g fis es fis] 
  )d4 \tiny fis8*1/2 ~ \normalsize gis4*3/4 ~ gis8 r r4 r2
  \tiny a8*1/2 ~ \normalsize f4*3/4 ~ f8 r r2 r
  r2 r4 [a8( b][c d c b] \tiny b8*1/2 ~ \normalsize 
  < { [e8*1/2 )g,8] } \context Voice=x { \stemup s8*1/4 \tiny a8*1/2 ~ } > r4 r2 r
  r2 r4 [a8( b][c d c b] [a b c d][c b a b][c d c b]
  \tiny b8*1/2 ~ \normalsize 
  < { [e8*1/2 )g,8] } \context Voice=x { \stemup s8*1/4 \tiny a8*1/2 ~ } > r4 r2 r
  a2( \tiny e'8*1/2 ~ \normalsize f4*3/4 ~ )f8 r r2
  r2 r r
  fis,4( \tiny dis8*1/2 \normalsize <)cis4*3/4 ais> r2 r
  \tiny b'8*1/2 ~ \normalsize 
  < { [a8*1/2 a8] } \context Voice=x { \stemup s8*1/4 \tiny b8*1/2 ~ } > r4 r2 r
  r4 [a'8--(\< a--] [a-- a-- c-- \!b--] [a--\> gis f \!e]
  < { [es8 )c] } \context Voice=x { \stemup s8*1/2 \tiny b8*1/2 ~ } > r4 r2 r
  d,4( \tiny fis8*1/2 ~ \normalsize gis4*3/4 ~ )gis8 r r4 r2
  f4 ~ f8 r r2 r
  [f'8( g a b][a g f g][a b a g]
  \tiny f8*1/2 ~ \normalsize 
  < { [g8*1/2 )e8] } \context Voice=x { \stemup s8*1/4 \tiny d8*1/2 ~ } > r4 r2 r
  [f8( g a b][a g f g][a b a g]
  \tiny f8*1/2 ~ \normalsize 
  < { [g8*1/2 )e8] } \context Voice=x { \stemup s8*1/4 \tiny d8*1/2 ~ } > r4 r2 r
  a,2( \tiny e'8*1/2 ~ \normalsize f4*3/4 ~ )f8 r r2
  r2 r r
  fis,4( \tiny dis8*1/2 \normalsize <)cis4*3/4 ais> r2 r
  <e1 g b e> ~ <e g b e>
}

basloopje = \notes\relative c{
  d,8( a' d f a d f d a f d )a
}

bassbeam = \notes{
  [s2] [s8 \translator Staff=treble s s s] [\translator Staff=bass s2]
%   [s2] [s2] [s2]
}

lower = \context Voice=two \notes \relative c{
  \stemdown
  \property Staff.slurVerticalDirection = 1

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
    \context GrandStaff < 
      \context Staff = treble < 
        \global 
	\upper
      >
      \context Staff = bass <
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
    slur_slope_damping = 100.0;

    \translator{ \OrchestralScoreContext }
    \translator{ 
      \GrandStaffContext
      minVerticalAlign = 3.0*\staffheight;
      maxVerticalAlign = 3.0*\staffheight;
    }
    \translator{ 
      \StaffContext
      % don't auto-generate bars: not a good idea: -> no breakpoints
      % barAuto = "0";
      % urg defaultBarType = "";
      defaultBarType = "empty";
      \remove "Time_signature_engraver";
    }
  }
  \midi {
    \tempo 4 = 54;
  }
}

