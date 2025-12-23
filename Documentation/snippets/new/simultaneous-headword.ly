\version "2.25.18"

\header {
  categories = "Headword"

  texidoc = "
Simultaneous headword.
"

  doctitle = "Simultaneous headword"
}

\include "english.ly"

% L. v. Beethoven, Op. 111
% Piano sonata 32
% Movt II - Arietta - Adagio molto semplice e cantabile
% measures 108 - 118

\layout {
  \context {
    \Score
  }
}

trillFlat =
\once \override TrillSpanner.bound-details.left.text = \markup {
  \concat {
    \musicglyph "scripts.trill"
    \translate #'(-0.5 . 1.9)
      \fontsize #-7 \flat
  }
}

\new PianoStaff <<
  \new Staff = "right hand" <<
    \set Score.currentBarNumber = #108
    \new Voice = "right hand 1" {
      \clef treble
      \key c \major
      \time 9/16

      \grace s32 s4. s8. |
      s4. \voiceOne a''8[(\p g''16]) |
      g''4.\dim af''8[( g''16]) |
      g''8.[ g''8. g''8.] |
      g''8.[\pp af''8.af''8.] |
      af''8.[ af''8.af''8.] |

      \trillFlat af''4.\startTrillSpan\< ~ af''8. ~ |
      af''4.\> ~ af''8. ~ |
      \oneVoice <af'' d''>8.[\p\cresc a''8. bf''8.] ~ |
      bf''8.[ b''8. c'''8.] ~ \bar "||"
      \key ef \major c'''8.[ cs'''8.] <>\stopTrillSpan <>\!
    }

    \new Voice = "right hand 2" {
      \override Voice.TrillSpanner.direction = #DOWN
      \grace cs''32 \voiceTwo d''4.\f\startTrillSpan ~ d''8. ~ |
      d''4. ~ d''8. ~ |
      d''8. <>\stopTrillSpan\trillFlat d''4.\startTrillSpan ~ |
      d''4. ~ d''8. ~ |
      d''4. ~ d''8. ~ |
      d''4. ~ d''8. ~ <> \stopTrillSpan |

      \trillFlat d''4.\startTrillSpan ~ d''8. ~ |
      d''4. ~ d''8. ~ |
      \once \override NoteColumn.ignore-collision = ##t
        \hideNotes d''8.\stopTrillSpan s4. |
      s4. s8. |
      s4.
    }
  >>

  \new Staff = "left hand" {
    \clef bass
    \key c \major
    \time 9/16

    \grace s32 r8. r8. <c! c,!>8[(\tweak X-offset #-2 _\f <g, g,,>16]) |
    <g, g,,>4. \clef treble c''8[( b'16]) |
    b'4. c''8[( b'16]) |
    b'8.[ b'8. b'8.] |
    b'8.[ bf'8.] \clef bass <f f,>8[( <bf, bf,,>16]) |
    <bf, bf,,>4. \clef treble f'8[( bf16]) |

    <<
      \new Voice {
        \voiceOne
        \override Voice.TrillSpanner.direction = #UP
        f'4.~ \startTrillSpan f'8.~ |
        f'4.~ f'8.~ |
        f'8. <> \stopTrillSpan
      }
      \new Voice {
        \voiceTwo
        \override Voice.TrillSpanner.direction = #DOWN
        bf8.[ bf8. bf8.] |
        bf8.[ bf8. bf8.] |
        bf8.
      }
    >> \oneVoice r8. r8. |
    R1*9/16 \clef bass |
    \key ef \major r8. r8.
  }
>>
