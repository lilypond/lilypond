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
 Tested Features: cross staff auto beams and slurs, grace notes, no bars
%}

\version "1.2.0";

\include "nederlands.ly"

global = \notes {
  \key a \minor;
  \time 6/4;
  \skip 1.*34;
  \bar ".|";
}
  
melody = \notes\relative c''{
  \clef violin;
  \property Voice.verticalDirection = 1
  \property Voice.graceFraction = "1/4"
  r2 r r 
  r2 r r
  r4 a'8--(\< a--  a-- a-- c-- \!b-- a--\> gis f \!e 
  es8 \grace b( ))c r4 r2 r
  r2 r r
  r4 a'8--(\< a--  a-- a-- c-- \!b-- a--\> gis f \!e 
  es8 \grace b( ))c r4 r2 r
  r4 g16( a bes a  g a bes a g a bes a g a bes a g fis es fis 
  )d4 \grace fis8()gis4 ~ gis8 r r4 r2
  r4 g16( a bes a  g a bes a g a bes a g a bes a g fis es fis 
  )d4 \grace fis8()gis4 ~ gis8 r r4 r2
  \grace a8()f4 ~ f8 r r2 r
  r2 r4 a8( b c d c b \grace b8()e \grace a,())g r4 r2 r
  r2 r4 a8( b c d c b  a b c d c b a b c d c b 
  \grace b8()e \grace a,())g r4 r2 r
  a2( \grace e'8()f4 ~ )f8 r r2
  r2 r r
  fis,4( \grace dis8<)cis4 ais> r2 r
  \grace b'8()a \grace b()a r4 r2 r
  r4 a'8--(\< a--  a-- a-- c-- \!b--  a--\> gis f \!e 
  es8 \grace b())c r4 r2 r
  d,4( \grace fis8()gis4 ~ )gis8 r r4 r2
  f4 ~ f8 r r2 r
  f'8( g a b a g f g a b a g 
  \grace f8()g \grace d)e r4 r2 r
  f8( g a b a g f g a b a g 
  \grace f8()g8 \grace d())e r4 r2 r
  a,2( \grace e'8() f4 ~ )f8 r r2
  r2 r r
  fis,4( \grace dis8<)cis4 ais> r2 r
  <e1 g b e> ~ <e g b e>
}

basloopje = \notes\relative c{
%  d,8( a' d f a \translator Staff=treble d f d \translator Staff=bass a f d )a
  d,8( a' d f a d f d a f d )a
}

accompany = \notes \relative c{
  % snapnie, hoevaak relative c heeft ze nodig?
  \notes\relative c \basloopje
  \notes\relative c \basloopje
  \notes\relative c \basloopje
  \transpose bes \notes\relative c{ \basloopje }
  \transpose bes \notes\relative c{ \basloopje }
  \notes\relative c \basloopje
  \transpose bes \notes\relative c{ \basloopje }
  \transpose bes \notes\relative c{ \basloopje }
  \transpose a \notes\relative c{ \basloopje }
  \transpose bes \notes\relative c{ \basloopje }
  \transpose a \notes\relative c{ \basloopje }
  \notes\relative c \basloopje
  \notes\relative c \basloopje
  % huh? d'
  \transpose d' \notes\relative c{ \basloopje }
  \notes\relative c \basloopje
  \notes\relative c \basloopje
  \transpose d' \notes\relative c{ \basloopje }
  \notes\relative c \basloopje
  \notes\relative c \basloopje
  \transpose e' \notes\relative c{ \basloopje }
  \notes\relative c \basloopje
  \notes\relative c \basloopje
  \transpose bes \notes\relative c{ \basloopje }
  \transpose a \notes\relative c{ \basloopje }
  \notes\relative c \basloopje
  \notes\relative c \basloopje
  \transpose d' \notes\relative c{ \basloopje }
  \notes\relative c \basloopje
  \transpose d' \notes\relative c{ \basloopje }
  \notes\relative c \basloopje
  \notes\relative c \basloopje
  \transpose e' \notes\relative c{ \basloopje }
  < e1 b' e> ~ < e b' e> 
}


\score{
  \notes{
    \context PianoStaff \relative c <
      %\context Staff=upper <
      \context Staff=up <
        \global
        \context Voice=foo {
	  \property Voice.verticalDirection = 1
	  \property Voice.scriptVerticalDirection = 1
	  \melody 
	}
      >
      \context Staff=down <
        \global
	\clef bass;
        \accompany
      >
    >
  }
  \paper {
    gourlay_maxmeasures = 4.;
    indent = 8.\mm;
    textheight = 295.\mm;

    slur_interstaff_slope_damping = 100.0;
    slur_interstaff_height_damping = 100.0;

    \translator{ 
      \StaffContext
      % don't auto-generate bars: not a good idea: -> no breakpoints
      % barAuto = "0";
      % urg defaultBarType = "";
      defaultBarType = "empty";
      \remove "Time_signature_engraver";

      slurVerticalDirection = 1;
      verticalDirection = -1;
      beamAutoEnd = "1/2";
    }
  }
  \midi {
    \tempo 4 = 54;
  }
}
