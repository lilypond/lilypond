\version "2.18.0"

\paper {
  tagline = ##f
  paper-height = 70\mm
}

measIu = {
  \override TupletBracket.outside-staff-priority = #500
  \set subdivideBeams = ##t
  \tupletUp
  \tweak style #'dashed-line
  \tuplet 3/2 {
    \tuplet 3/2 {
        \override NoteHead.color = #red
        \once \override DynamicLineSpanner.outside-staff-priority = ##f
        d'4 -\tweak X-offset #-2 -\tweak Y-offset #0 ^\ff r8
      }
   r8
  }
}

measIl = {
  \set subdivideBeams = ##t
  \tupletDown
  \override NoteHead.color = #red
  <<
    {
      \voiceOne
      \override NoteHead.color = #red
      ees4 ^\f
    }
    \new Voice {
      \voiceTwo
      \override NoteHead.color = #red
      \override TupletBracket.outside-staff-priority = #500
      \tweak style #'dashed-line
      \tuplet 3/2 {
        f,4~ -\tweak X-offset #-2 _\f
        \tuplet 3/2 {
          f,16 r8
        }
      }
    }
  >>
  \oneVoice
  | %1
}

measIIu = {
  \override TupletBracket.positions = #'(10.5 . 10.5)
  \tupletUp
  \tuplet 5/4 {
    \override NoteHead.color = #blue
    b'''16[ -\tweak X-offset #-4.5 ^\mf
    a''16.~^\ff
  }
  \tuplet 5/4 {
    \override NoteHead.color = #blue
    a''32
    \override NoteHead.color = #red
    d'''8^\ff
  }
  \tuplet 5/4 {
    \override NoteHead.color = #blue
    r16. << g''16] aes'''16]^\p >>
  }
  | % 2
}

measIIl = {
  \set subdivideBeams = ##t
  \tupletUp
  \clef treble
  \tuplet 5/4 {
    \override NoteHead.color = #blue
    \set stemRightBeamCount = #1
    r16.[ gis'16~_\f
  }
  \tuplet 5/4 {
    gis'32
    \override NoteHead.color = #red
    c'8_\pp
  }
  \tuplet 5/4 {
    cis''16. _\mf
    \override NoteHead.color = #blue
    fis'16] _\p
  }
}

measIIIu = {
  \tupletUp
  \override TupletBracket.bracket-visibility = ##t
  \tweak style #'dashed-line
  \tweak text #tuplet-number::calc-fraction-text
  \tweak positions #'(20 . 20)
  \tuplet 4/3 {
    \tweak positions #'(17.5 . 17.5)
    \tuplet 5/4 {
      \override NoteHead.color = #red
      \change Staff = lower
      \voiceOne
      <d'' es'> 8[ _\laissezVibrer
      \change Staff = upper
      \voiceTwo
      f''32^\f\laissezVibrer
    }
    \override NoteHead.color = #blue
    b''32^\ff\laissezVibrer
    a'16.^\ff\laissezVibrer
    gis''8^\ff\laissezVibrer
    \change Staff = lower
    \voiceOne
    \clef bass
    \override NoteHead.color = #red
    cis'8]_\pp
    \oneVoice
  }
}

measIIIl = {
  \textSpannerDown
  \override TextSpanner.bound-details.left.text = \markup { \musicglyph #"pedal.Ped" }
  \override TextSpanner.bound-details.right.text = \markup { \musicglyph #"pedal.*" }
  \override TextSpanner.dash-fraction = #0.05
  \override TextSpanner.dash-period = #1
  s8 _\pp\startTextSpan s8
  s16 s32 s32 \stopTextSpan
}

measIVu = {
  \revert TupletBracket.positions
  \override NoteHead.color = #red
  \change Staff = upper
  \tupletUp
  c''4.~_\p
  \tuplet 5/4 {
    c''32 r8
  }
}

measIVl = {
  \override NoteHead.color = #red
  d,4.~_\mf
  \tuplet 5/4 {
    d,32 r8
  }
}

\score {
  \new PianoStaff <<
    \new Staff = "upper"
    {
      \hide Staff.TimeSignature
      \override Staff.TimeSignature.extra-spacing-width = #'(0.0 . 3.0)
      \accidentalStyle dodecaphonic
      \autoBeamOff
      \clef treble
      \measIu
      \measIIu
      \measIIIu
      \measIVu
    }
    \new Dynamics \with { \consists "Time_signature_engraver" }
    {
      \override Dynamics.TimeSignature.font-size = #4
      \override Dynamics.TimeSignature.font-name = "New Century Schoolbook"
      \time 2/8
      s4 |
      \time 3/8
      s4. |
      s4. |
      \time 4/8
      s2
    }
    \new Staff = "lower"
    {
      \hide Staff.TimeSignature
      \accidentalStyle dodecaphonic
      \autoBeamOff
      \clef bass
      \measIl
      \measIIl
      \measIIIl
      \measIVl
    }
  >>
}
