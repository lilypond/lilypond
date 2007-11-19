\version "2.11.34"
\include "english.ly"

% NR 1.1 Pitches

% L. v. Beethoven 
% Piano sonata 21 - Dem Grafen von Waldstein Gewidmet
% chorale at measures 34 - 40+

\paper {
  #(define dump-extents #t)
  indent = 0\mm
  ragged-right = ##t
  line-width = 16\cm
  force-assignment = #""
  line-width = #(- line-width (* mm  3.000000))
}

\layout {
   \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = 
         #(ly:make-moment 1 20) 
   }
}

\new PianoStaff <<

   % RH Staff
   \new Staff <<

      % RH Voice 1
      \new Voice {
         \set Score.currentBarNumber = #34
         \voiceOne
         gs''2 ( ^ \markup \italic { dolce e molto ligato }
         fs''4
         e''4
         |
         ds''2
         cs''2 )
         |
         ds''2 (
         e''4
         fs''4
         |
         <gs'' e''>2
         <fs'' ds''>2 )
         |
         \oneVoice
         \clef bass
         <gs' e' b>2 (
         <fs' ds' a>4
         <e' cs' gs>4
         |
         <ds' bs fs>2
         <cs' a e>2 )
         |
         \voiceOne
         b2 %(
         cs'4
         ds'4
         |
         \clef treble
         <e' gs>4 %)
      }

      % RH Voice 2
      \new Voice {
         \voiceTwo
         \override Staff.DynamicLineSpanner #'staff-padding = #2.5
         <e'' b'>2 \p
         <ds'' a'>4
         <cs'' gs'>4
         |
         <bs' fs'>2
         e'2
         |
         \once \override TextScript #'staff-padding = #2.5
         <b'! a'>2 _ \markup \italic { cresc. }
         b'4
         <e'' cs''>4
         |
         b'2. ( \sf \>
         a'4 ) 
         |
         \clef bass
         s1 \p
         |
         s1
         |
         <gs e>4 (
         <a fs>2. )
         |
         s4
      }

   >>

   % LH Staff
   \new Staff {
      \override Staff.SustainPedalLineSpanner #'staff-padding = #5
      <gs' e'>2 ( \sustainDown
      <fs' ds' b>4 \sustainUp
      <e' cs'>4
      |
      <ds' bs gs>2
      <cs' a>2 ) \sustainDown
      |
      \clef bass
      \slurDown
      <ds' b! a fs>2 ( \sustainUp
      <e' b gs>4
      <fs' cs' a>4 \sustainDown
      |
      \clef treble
      \voiceOne
      <<
         {
            <gs' e'>2
            <fs' ds'>2 )
         }
         \new Voice {
            \voiceTwo
            b1 \sustainUp
         }
      >>
      \oneVoice
      |
      %\break
      \clef bass
      <gs e>2 ( 
      <fs ds b,>4
      <e cs>4
      |
      <ds bs, gs,>2
      <cs a,>2 ) \sustainDown
      |
      <b,! b,,!>1 ( \sustainUp
      |
      <e e,>4 )
   }

>>
