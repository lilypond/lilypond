\version "2.11.51"
\include "english.ly"
#(set-global-staff-size 15)
\paper{
  ragged-right=##t
  line-width=17\cm
  indent=0\cm
}
 
\layout {
  \context { \Score
    \override PaperColumn #'keep-inside-line = ##t
    \override NonMusicalPaperColumn #'keep-inside-line = ##t
  }
}

% NR 1.6 Staff notation

% L. v. Beethoven, Op. 106
% Piano sonata 29 - Für das Hammer-Klavier
% Movt IV

\layout {
   \context {
      \Score
      %\override SpacingSpanner #'base-shortest-duration = #(ly:make-moment 1 16) 
      \override Beam #'breakable = ##t
      \override BarNumber #'transparent = ##t
   }
}

\new PianoStaff <<

   % RH staff
   \new Staff {
      \clef treble
      \key ef \major
      \time 3/4

      <<

         % RH voice 1
         \new Voice {
            \partial 1 * 5/8
            \voiceOne
            \once \override DynamicLineSpanner #'staff-padding = #4
            c'''16 \f (
            e'''16
            f'''16
            g'''16
            af'''16
            g'''16
            f'''16
            ef'''16
            d'''16
            c'''16 )

            |

            s8
            \once \override TextScript #'staff-padding = #6
            d''8 [
            ^ \markup \italic { non pressare }
            \override Voice.Script #'extra-offset = #'(0 . 1)
            b8 ] \trill
            r8
            r8
            g''8 [

            |

            ef'8 ] \trill
            r8
            r8
            c'''8 [
            a'8 ] \trill
            r8

            |

            r8
            d''' [
            bf'!8 ] \trill
            r8
            r8
            g'''8 [

            \bar "||"
            \break
            \key d \minor

            e''8 ] \trill
            r8
            r8

         }

         
         % RH voice 2
         \new Voice {
            \voiceTwo
            \partial 1 * 5/8
            r8
            c'''4.
            fs''8 \staccato

            |

            <b'' g''>8 \staccato
            r8
            r8
            g8 [
            b'8 ] \trill
            r8

            |

            r8
            c'8 [
            ef''8 ] \trill
            r8
            r8
            d'8 [

            |

            fs''8 ] \trill
            r8
            r8
            g'8 [
            bf''!8 ] \trill
            r8

            \bar "||"
            
            r8
            cs''8 [
            e'''!8 ] \trill

         }

      >>

      \oneVoice
      <a''' a''>8 [
      <f''' f''>8
      <d''' d''>8 ]

      |

      <a'' a'>8 [ \sf
      <f'' f'>8 ]
      <d'' d'>8 [ \sf
      <a' a> ]
      <gs' d' gs> [ \sf
      <gs'' d'' gs'> ]

      |

      \override DynamicLineSpanner #'staff-padding = #4
      \override DynamicText #'staff-padding = #4
      <a'' cs'' a'> \ff \staccato
      r8
      <a''' e''' cs''' a''>8 \ff \staccato
      r8
      r4

      |

      R1 * 3/4

      \bar "||"
      %\key d \major
   }

   % LH
   \new Staff {
      \clef bass
      \key ef \major
      \time 3/4

      \partial 1 * 5/8
      r8
      a2 \sf \startTrillSpan

      |

      g8 \staccato \stopTrillSpan
      b8 [
      g,8 ] \trill
      r8
      r8
      ef'8 [

      |

      c8 ] \trill
      r8
      r8
      \stemDown
      a8 [
      fs,8 ] \trill
      r8

      |

      r8
      bf!8 [
      g,8 ] \trill
      r8
      r8
      e'8 [

      \bar "||"
      \key d \minor

      cs8 ] \trill
      r8
      r8
      a8 [
      f8
      d8 ]

      |

      a,8 [
      f,8 ]
      d,8 [
      <a, a,,>8 ]
      <bf, bf,,>8 [
      <bf, bf,,>8 ]

      |

      <a, e, cs, a,,>8 \staccato
      r8
      <a, e, cs, a,,>8 \staccato
      r8
      r4

      |

      R1 * 3/4

      \bar "||"
      %\key d \major
   }

>>

