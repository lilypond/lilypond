

#(set! point-and-click line-column-location)

forcedBreak = \notes { }
%%forcedBreak = \notes { \break }
forcedLastBreak = \notes { \break }
%%forcedLastBreak = \notes { }

%% We want this to perfectly match the Baerenreiter spacing.
%% If we're not using 6 systems, there's definately a problem.
#(define (assert-system-count smob n)
  (let ((systems (length (get-broken-into
			  (get-original
			   (get-system smob))))))
    (if (not (equal? n systems))
	;; Can't use error yet, as we know that we're not using 6...
	;;(error
	(warn
	(string-append "Got " (number->string systems)
			     " systems (expecting " (number->string n))))))
            

\header {
  title = "Solo Cello Suite II"
  piece ="Sarabande"
  composer = "J.S.Bach"
  editor = "August Wenzinger"
  source= "B\\\"arenreiter Urtext"

  texidoc = "The B\\\"arenreiter edition of the Cello Suites is the
most beautifully typeset piece of music in our collection of music (we
both own one. It is also lovely on French Horn). This piece does not
include articulation, but it does follows the same beaming and
linebreaking as the printed edition. This is done in order to
benchmark the quality of the LilyPond output.

As of lilypond 1.5.42, the spacing and beam quanting is almost
identical.

There are two tweaks in this file: a line-break was forced before
measure 25, we get back the linebreaking of Baerenreiter.  The stem
direction is forced in measure 24. The last beam of that measure is up
in Baerenreiter because of context. We don't detect that yet.

Note that the Barenreiter edition contains a few engraving
mistakes. The second line begins with measure 6 (but prints 5). The |:
half way in measure 13 has been forgotten.
 "
}


\version "1.5.68"


sarabandeA =  \context Voice \notes \relative c {
  \property Staff.NoteCollision \set #'merge-differently-dotted = ##t
  < { d8. e16 e4.-\trill d16 e } \\
    { d4 a2 } >
  f4.  [e8 d c] |
  [bes g'] [f e16(f] [g a bes)d,] |
  cis4.-\trill [b8 a g] |

  %% check spacing without accs: 
  %%	c4.-\trill [bes8 a g] |
  
  < { d'8. e16 f4.-\trill d16 e |
      f4. [d8 e f] }
    \\
    { <a,4 f> a2 <a4. d,4.>  } > |

  %%7
  g8 bes16()a c()bes a()g d'8 f, |
  <  e4.-\trill
  \\ <c,4 g'> >
  [d8 c bes]

  %%9
  < { f'8 g16()a a4. g16()f  |
      g8 a16()bes bes4. c16()d }
    \\
    { a,4 <bes4. d4. > r8 bes4 <g2 f'2>  }
  > |
  \forcedBreak

  %% 11
  [e,8 f] [c, g'] [f' e] |
  f4 f,2 |
  < {  a'4 a4.-\trill bes8 
       c bes16 a } \\
    { [f8 es] es4. r8 d4 } >

  fis8.-\trill es16 d8 c |
  [bes g']
  [a, fis']
  [es' d] |
  \forcedBreak
  
  %%16
  < bes4.-\trill d, g, > [a8 g f!] |
  e bes a f' g a |
  d, as g es' f g |
  [cis, bes'] [a g16 f] [e!8 f16 d] |
  cis8 e16 a a,8. g'16 f8()e |
  \forcedBreak
  
  %%21
  < { d e16()f f4. e16()d |
      e8 f16()g g4. a16()bes |
      a8 cis16 d d,8 e16 f32 g f8-\trill e16()d } \\
    { bes4 g2 |
      g4 <bes4. cis,> s8 |
      <d8 a f> r r g, a4 } >
  |
  \stemUp
  d4 d,16 a'( b cis d e f )g |
  \stemBoth
  \forcedLastBreak
  %%25
  < { a16(b c)b c4. b16()a |
      b cis d cis d4. e16()f | }
    \\
    { f,4 fis4. s8 |
      <d4 g,> gis4.   } >
  \voiceOne
  d16(cis)d f,
  [a,8 e']
  \oneVoice
  [d' cis] |
  %%  d4 d,,2 |
  d4
  \property Thread.NoteHead
  \override #'after-line-breaking-callback
  = #(lambda (smob) (assert-system-count smob 6.1))
  d,,2 |
}


sarabande =  \context Staff \notes<
  \apply #voicify-music \sarabandeA
  
>

\version "1.5.68"

sarabandeCelloGlobal =  \notes{
  \time 3/4
  \key f \major
  \clef bass
  \repeat "volta" 2 {
    s2.*12
  } \repeat "volta" 2 {
    s2.*16
  }
}

sarabandeCelloScripts =  \notes{
}

sarabandeCelloStaff =  \context Staff <
  \sarabande
  \sarabandeCelloGlobal
  \sarabandeCelloScripts
>

% size perversions
smallerPaper = \paper {
    \translator { \StaffContext
		  fontSize = #-1
		  StaffSymbol \override  #'staff-space = #0.8
		  }
    \translator { \ScoreContext
		   SpacingSpanner \override #'spacing-increment = #0.96
		}
	
    indent = 5.6 \mm
    linewidth = 146.8 \mm
}

baerPaper = \paper {
    indent = 7. \mm
    linewidth =183.5 \mm
    interscoreline=4.0\mm
    \translator {
	     \ScoreContext
%	     System \override #'molecule-callback = #box-grob-molecule
    }
}


\score{
  \sarabandeCelloStaff
  \paper{
    \baerPaper
  }
  \midi{ \tempo 4 = 40 }
  \header{
    opus= "" 
    piece ="Sarabande" }
}

%%% Local variables:
%%% LilyPond-indent-level:2
%%% End:
