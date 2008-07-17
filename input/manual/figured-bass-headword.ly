\version "2.11.51"
#(set-global-staff-size 15)
\paper{
  ragged-right=##f
  line-width=16\cm
  indent=1.5\cm
}

\layout {
  \context { \Score
    \override PaperColumn #'keep-inside-line = ##t
    \override NonMusicalPaperColumn #'keep-inside-line = ##t
  }
}

% NR 2.7.3 Figured bass

% Arcangelo Corelli, 12 Sonate da Camera, Op. 2
% Sonata II, Allemanda
% measures 1 - 88
% Coded by Neil Puttock; modified by Carl Sorensen

extendOn = \bassFigureExtendersOn
extendOff = \bassFigureExtendersOff

\score {
  
  \new StaffGroup <<
  
    \new GrandStaff <<
      
      \new Staff = "violinoI" {
        \set Staff.instrumentName = \markup {
          \hcenter-in #11
          \line { Violino I. }
        }
        \time 4/4
        \mark \markup { \italic Adagio. }
        \partial 8
        r16 a'16 |
        a'8. [ d''16 d''8.  e''16 ] cis''8 a'4 a''16 bes''16 |
        cis''8 d''16 ( e'' ) e''8.  d''16 d''4 r8 d''16 e''16 |
        f''8 f''4 g''16 ( f''16 ) e''8 e''4 f''16 ( e''16 ) |
        d''8.  d''16 g''16 ( f''16 ) e''16 ( d''16 ) cis''8 
            cis''4 cis''16 cis''16 |
        d''8 d''8 c''8.  c''16 c''8 ( b'4 ) b'16 b'16 |
        c''8 c''8 bes'8.  bes'16 bes'8 ( a'4 ) a''16 a''16 |
        a''8 g''8 g''8.  g''16 g''8 ( f''8 ) r8 f''8 |
      }
      
      \new Staff = "violinoII" {
        \set Staff.instrumentName = \markup {
          \hcenter-in #11
          \line { Violino II. }
        }
        \time 4/4
        \partial 8
        r16 f'16 |
        f'8.  g'16 g'4 a'4 r8 d''16 d''16 |
        e''8 a'8 cis''8.  d''16 d''4 r8 f''16 g''16 |
        a''8 a''8 d''8.  d''16 g'8 g'8 c''8.  c''16 |
        f'8.  f''16 bes''16 ( a''16 ) g''16 ( f''16 ) e''8 e''4 e''16 e''16 |
        a'8 fis''8 g''8 a''8 d''8 d''4 d''16 d''16 | 
        g'8 e''8 f''8 g''8 c''8 c''4 cis''16 cis''16 | 
        d''8 d''8 e''8.  e''16 e''8 a'8 r8 d''8 | 
      }
      
    >>
    
    \new Staff = "violone" {
      \set Staff.instrumentName = \markup {
        \hcenter-in #13 {
          \center-align {
            Violone,
            \line { e Cembalo. }
          }
        }
      }
      \time 4/4
      \clef bass
      \partial 8
      r16 d16 | 
      d4 bes,4 a,4 f4 | 
      g8 f16 g16 a8 a,8 d4 d'4 ~ | 
      d'8 c'8 b4 c'8 c'16 bes16 a4 | 
      bes8 bes16 a16 g4 a8 a,4 a16 g16 |
      fis8 d8 e8 fis8 g8 g,4 g16 f16 |
      e8 c8 d8 e8 f8 f,4 a,8 | 
      b,4 cis4 d4 r8 d'8 | 
    }
    
    \new FiguredBass \figuremode {
      \set figuredBassAlterationDirection = #RIGHT
      \set figuredBassPlusDirection = #RIGHT      
      \override VerticalAxisGroup #'minimum-Y-extent = #'()
      \override BassFigureAlignment #'stacking-dir = #DOWN
      s8 |
      s4 <6>4 <_+>4 <6>4 | 
      <6 4\+ 2>8 <6>8 <_+> s8 s2 |
      <5>8 <6 4>8 <6 5>4 s4 <5>8 <6>8 |
      s4 <6 5 _-> <_+>2 |
      <6>8 <_+>8 <6>8 <6 5>8 <5 4>8 \extendOn <5 _!>8 \extendOff s4 |
      <6>4 <6->8 <6 5->8 <5 4->8 \extendOn <5 3>4 \extendOff <5 _+>8 |
      <7>8 <6>8 <5>4 <9 4>8 <8 3>8 s4 |
    }
    
  >>
  
  \layout {
    \context {
      \Score
      \override RehearsalMark #'break-align-symbols = #'(time-signature)
      \override RehearsalMark #'self-alignment-X = #LEFT
      \override TimeSignature #'break-align-anchor-alignment = #LEFT
    }
  }
}
