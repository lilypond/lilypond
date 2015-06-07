\version "2.19.21"

forcedLastBreak =  {} %%  { \break } if needed to match original breaks

%% We want this to perfectly match the Bärenreiter spacing.
%% If we're not using 6 systems, there's definitely a problem.
#(define (assert-system-count smob n)
  (let ((systems (length (ly:spanner-broken-into
			  (ly:grob-original
			   (ly:grob-system smob))))))
    (if (not (equal? n systems))
	(error
	;;(warn
	(string-append "Got " (number->string systems)
			     " systems (expecting " (number->string n))))))


#(define (assert-system-count-override count)
  #{ \override NoteHead.after-line-breaking
       = #(lambda (smob) (assert-system-count smob count))
  #})


\header {
  title = "Solo Cello Suite II"
  piece ="Sarabande"
  composer = "Johann Sebastian Bach (1685-1750)"
  editor = "August Wenzinger"
  source= "Bärenreiter Urtext"

  texidoc = "The Bärenreiter edition of the Cello Suites is the
most beautifully typeset piece of music in our collection of music (we
both own one. It is also lovely on French Horn). This piece does not
include articulation, but it does follows the same beaming and
linebreaking as the printed edition. This is done in order to
benchmark the quality of the LilyPond output.

As of lilypond 1.5.42, the spacing and beam quanting is almost
identical.

There are two tweaks in this file: a line-break was forced before
measure 25, we get back the linebreaking of Bärenreiter.  The stem
direction is forced in measure 24. The last beam of that measure is up
in Bärenreiter because of context. We don't detect that yet.

Note that the Bärenreiter edition contains a few engraving
mistakes. The second line begins with measure 6 (but prints 5). The |:
half way in measure 13 has been forgotten.
 "
}


sarabandeA =  \context Voice  \relative {
  \override Staff.NoteCollision.merge-differently-dotted = ##t


  << { d8. e16 e4.\trill d16 e } \\
    { d4 a2 } >>
  <d, a' f'>4.  e'8[ d c] |
  bes[ g'] f[
     e16(f] g[ a bes d,)] |
  cis4.\trill b8[ a g] |

  %% check spacing without accs:
  %%	c4.\trill bes8[ a g] |

  << { d'8. e16 e4.\trill d16 e |
      f4. d8[ e f] }
    \\
    { <a, f>4 a2 <a d,>4.  } >> |

  %%7
  g'8 bes16(a) c(bes) a(g) d'8 f, |
  <<  e4.\trill
    \\ <c, g'>4 >>
  d'8[ c bes]

  %%9
  << { f'8 g16(a) a4. g16(f)  |
      g8 a16(bes) bes4. c16(d) }
    \\
    { a,4 <bes d >4. r8 bes4 <g f'>2  }
  >> |

  %% 11
  e'8[ f] c,[ g'] f'[ e] |
  f4 f,2 |
  << {  a'4 a4.\trill bes8
       c bes16 a } \\
    { f8[ es] es4. r8 d4 } >>

  fis8.\trill es16 d8 c |
  bes[ g']
  a,[ fis']
  es'[ d] |

  %%16
  < bes d, g, >4.\trill a8[ g f!] |
  e bes a f' g a |
  d, as g es' f g |
  cis,[ bes'] a[ g16 f] e!8[ f16 d] |
  cis8 e16 a a,8. g'16 f8(e) |

  %%21
  << { d e16(f) f4. e16(d) |
      e8 f16(g) g4. a16(bes) |
      a8 cis16 d d,8 e16 f32 g f8\trill e16(d) } \\
    { bes4 g2 |
      g4 <bes cis,>4. s8 |
      <d a f>8 r r g, a4 } >>
  |
  \stemUp
  d4 d,16 a'( b cis d e f g) |
  \stemNeutral
  \forcedLastBreak
  %%25
  << { a16(b c b) c4. b16(a) |
      b cis d cis d4. e16(f) | }
    \\
    { f,4 fis4. s8 |
      <d g,>4 gis4.   } >>
  \voiceOne
  d'16(cis  d) f,
  a,8[ e']
  \oneVoice
  d'[ cis] |
  %%  d4 d,,2 |
  d4
%  $(assert-system-count-override 6)
  d,,2 |
}


sarabandeCelloGlobal = {
  \time 3/4
  \key d \minor
  \clef bass
  \repeat "volta" 2 {
    s2.*12
  } \repeat "volta" 2 {
    s2.*16
  }
}

sarabandeCelloScripts = {
}

sarabandeCelloStaff = \context Staff <<
  \sarabandeA
  \sarabandeCelloGlobal
  \sarabandeCelloScripts
>>

%% size perversions
smallerPaper = \layout {
  \context {
    \Staff
    fontSize = #-1
    \override StaffSymbol.staff-space = #0.8
  }
  \context {
    \Score
    \override SpacingSpanner.spacing-increment = #0.96
  }

  indent = 5.6 \mm
  line-width = 146.8 \mm
}

\paper {
  ragged-bottom = ##t
  indent = 7. \mm
  line-width =183.5 \mm
  system-system-spacing.basic-distance = 14.22 % 25mm, in staff-spaces
  system-system-spacing.padding = #0
  score-system-spacing.basic-distance = #0
  score-system-spacing.padding = #0
  system-count = 6

%%  annotatespacing = ##t
}

\book {
  \score{
    \sarabandeCelloStaff
    \layout { }

  \midi {
    \tempo 4 = 40
    }


    \header{
      opus= ""
      piece ="Sarabande" }
  }
}
%%% Local variables:
%%% LilyPond-indent-level:2
%%% End:

