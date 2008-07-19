\version "2.11.51"
\include "english.ly"
#(set-global-staff-size 15)
\paper{
  line-width = 16\cm
  indent = 0\cm
}
 
\layout {
  \context { \Score
    \override PaperColumn #'keep-inside-line = ##t
    \override NonMusicalPaperColumn #'keep-inside-line = ##t
  }
}

% NR 1.8 Text

% L. v. Beethoven, Op. 110
% Piano sonata 31
% measures 1 - 7

\layout {
   \context {
      \Score
      \override SpacingSpanner #'base-shortest-duration = 
         #(ly:make-moment 1 20) 
      %\override NonMusicalPaperColumn #'line-break-system-details =
      %#'((alignment-offsets . (0 -12)))
   }
}

\new PianoStaff <<

   % RH staff
   \new Staff {
      \clef treble
      \key af \major
      \time 3/4

      \once \override TextScript #'staff-padding = #4
      <c'' af'>4. (
      _ \markup { \dynamic p \italic { con amabilità } }
      <af' ef'>8 )
      <af' ef'>8. [
      <af' ef'>16 ]

      |

      <df'' g'>4
      <bf' g'>2
      _ \markup { \italic { ten. } }

      |

      <<

         \new Voice {
            \voiceOne
            ef''4. (
            df''8 [ )
            ef'' (
            f'' ) ]

            |

            \once \override TextScript #'staff-padding = #4
            ef''8. (
            _ \markup { \italic { tranqu. } }
            d''16 )
            df''8 \trill \fermata
            _ \markup { \italic { ten. } }
         }

         \new Voice {
            \voiceTwo
            af'4.
            af'8 [
            af'8
            af'8 ]

            |

            g'4
            g'8
         }

      >>

      \oneVoice
      \once \override TextScript #'staff-padding = #4
      df''32 
      _ \markup { \italic dolce }
      c''32 
      df''32
      ef''32
      \acciaccatura { df''[ ef'' ] }
      f''8
      ef''16
      df''16

      |
      \break
      \overrideProperty "Score.NonMusicalPaperColumn"
      #'line-break-system-details #'((alignment-offsets . (0 -12)))

      c''4. (
      ^ \markup \italic { cantabile, con intimissimo sentimento,
         ma sempre molto dolce e semplice }
      ef''4
      af''8 )

      |

      af''4 (
      g''2 )

      |

      bf''4. (
      g''4
      ef''8 )

      |
   }

   % LH
   \new Staff { 
      \clef bass
      \key af \major
      \time 3/4

      <ef af,>4. (
      <ef c>8 )
      <ef c>8. [
      <ef c>16 ]

      |

      <ef bf,>4
      <ef df>2 ^ \markup { \italic ten. }

      |

      <<

         \new Voice {
            \voiceOne
            ef8 [ (
            af8
            c'8
            bf8 )
            c'8 ( \staccato
            df'8 ]  ) \staccato

            |

            bf4
            bf8 ^ \markup { \italic ten. }
         }

         \new Voice {
            \voiceTwo
            c4. (
            f8 [ )
            ef8 ( \staccato
            df8 ] ) \staccato

            |

            ef4
            ef8 \fermata
         }

      >>

      \oneVoice
      r8
      r4
      \clef treble

      |

      \override Staff.SustainPedalLineSpanner #'outside-staff-priority = #1000
      \override Staff.SustainPedalLineSpanner #'staff-padding = #7
      \once \override TextScript #'padding = #2
      af16 \sustainOn
      ^ \markup \italic { non staccato }
      _ \markup \italic { molto \concat {\dynamic {p},} sempre tranquillo
         ed egualmente, non rubato }
      <ef' c'>16
      <ef' c'>16
      <ef' c'>16
      af16
      <ef' c'>16
      <ef' c'>16
      <ef' c'>16
      af16 \sustainOn
      <ef' c'>16
      <ef' c'>16
      <ef' c'>16

      |

      bf16 \sustainOn
      <ef' df'>16
      <ef' df'>16
      <ef' df'>16
      bf16 \sustainOn
      <ef' df'>16
      <ef' df'>16
      <ef' df'>16
      bf16
      <ef' df'>16
      <ef' df'>16
      <ef' df'>16

      |

      \override Staff.SustainPedalLineSpanner #'staff-padding = #4
      df'16 \sustainOn
      <bf' g' ef'>16
      <bf' g' ef'>16
      <bf' g' ef'>16
      df'16
      <bf' g' ef'>16
      <bf' g' ef'>16
      <bf' g' ef'>16
      df'16 \sustainOn
      <bf' g' ef'>16
      <bf' g' ef'>16
      <bf' g' ef'>16

      |
   }

>>
