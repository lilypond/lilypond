%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is a brief passage from Enrique Granados %
% Goyescas, "Coloquio en la Reja."              %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\version "2.19.83"

\include "example-header.ily"

#(set-global-staff-size 14)


\paper {
  paper-height = 70\mm
}


csh = \change Staff = "high"
csm = \change Staff = "middle"
csl = \change Staff = "low"

crmolto = \markup { \italic "cresc. molto" }
appassmolto = \markup { \whiteout \italic "appassionato molto" }

#(ly:set-option 'point-and-click #f)


global = { \time 3/4 }


upperVoiceOne = \relative {
  \override TupletBracket.bracket-visibility = ##t
  \override TupletBracket.tuplet-slur = ##t
  \override TupletNumber.padding = #0
  \override TupletBracket.padding = #0.9

  % 1
  \voiceOne
  <aes' f'>8\tweak height-limit #4 ([ \tuplet 5/4 { g'32 aes g f g] }
  <es aes>8[ \tuplet 5/4 { <d bes'>32 c' bes aes bes] }
  <es, aes es'>8 <d fis bes d>) |

  % 2
  <c g' c>4( \voiceTwo
  <bes es bes'>4)\arpeggio
  <aes aes'>16^( <bes bes'> <g g'>8) |

  % 3
  \voiceOne
  <f aes d f>8\tweak height-limit #4 ([ \tuplet 5/4 { <g g'>32 aes' g f g] }
  <aes, aes'>16
    \set stemRightBeamCount = #1
    <c f>
    \set stemLeftBeamCount = #1
    \tuplet 5/4 { bes'32 c bes aes bes] }
  \ottava #1 <es es'>16 <f f'> <d d'> \slashedGrace f8 <es es'>16) |
}

upperVoiceTwo = \relative {
  % 1
  \voiceTwo s8 c''8\<
  <bes, f'>[ <bes aes'>
  c' <bes, d fis>\!] |

  % 2
  s4_\tweak X-offset #1 -\appassmolto
  \voiceOne a'''8\rest <bes, es bes'>->
  s4 |

  % 3
  s8 \voiceTwo \crossStaff { g,8
  aes4 }
}

middleVoiceOne = \relative {
  % 1
  \crossStaff { d'!8 } s8
  s8 s8\offset Y-offset #-2 _\crmolto
  s4 |

  % 2
  s4
  <g bes>8[ <es' g>]
  \voiceOne e,8( dis16 e) |

  % 3
  \once \override VoiceFollower.bound-details.left.Y = #-10
  \showStaffSwitch \csh \stemUp f4
}

middleVoiceTwo = \relative {
  % 1
  s2. |

  % 2
  s4
  \hideNotes \stemUp bes'\arpeggio \unHideNotes
  \voiceTwo \crossStaff { e,!4 } |

  % 3
  s4
  <bes c es f>8 <f' aes es'>16 d'
  <bes, f' aes c>8 <bes' fis'> |
}

lowerVoiceOne = \relative {
  \mergeDifferentlyHeadedOn
  \mergeDifferentlyDottedOn

  % 1
  \once \override Beam.damping = #5
  bes,,8 \csm \stemDown <bes'' c es>8
  s2 |

  % 2
  \csl \stemUp s8
    \hideNotes \stemDown
    es,,64\tweak eccentricity #-1
          \tweak height-limit #5 ^( s64 s
    \unHideNotes \stemUp
    g'64\offset positions #'(-0.7 . -0.7)
        \tweak damping #3 [
    \set stemLeftBeamCount = #1
    bes c d c])
  s2 |

  % 3
  \once \override Beam.damping = #5
  bes,,8 \csm \stemDown <bes'' c es>8 s2 |
}

lowerVoiceTwo = \relative {
  % 1
  \voiceTwo bes,,2. |

  % 2
  \csh
  \once \override Beam.damping = #+inf.0
  % XXX Currently, cross-staff beaming support is partially broken.  This
  %     means we have to adjust the vertical beam position manually.
  \once \override Beam.positions = #'(-30 . -30)
  <bes'' es g>8 \csl es,,64 bes' es g s32. c64
  s4
  <bes des>4

  % 3
  bes,,2.
}


\score {
  \new PianoStaff <<
    \set PianoStaff.connectArpeggios = ##t
    \override PianoStaff.Arpeggio.stencil = #ly:arpeggio::brew-chord-bracket
    \override PianoStaff.Arpeggio.padding = #-0.5

    \new Staff = "high" <<
      \global
      \context Voice = "upperVoiceOne" { \upperVoiceOne }
      \context Voice = "upperVoiceTwo" { \upperVoiceTwo }
    >>
    \new Staff = "middle" <<
      \global
      \context Voice = "middleVoiceOne" { \middleVoiceOne }
      \context Voice = "middleVoiceTwo" { \middleVoiceTwo }
    >>
    \new Staff = "low" <<
      \clef bass
      \global
      \context Voice = "lowerVoiceOne" { \lowerVoiceOne }
      \context Voice = "lowerVoiceTwo" { \lowerVoiceTwo }
    >>
  >>

  \layout {
    \context {
      \Score
      \omit TimeSignature
      \remove Bar_number_engraver
    }
    \context {
      \PianoStaff
      \consists #Span_stem_engraver
    }
    \context {
      \Staff
      \override Accidental.extra-spacing-width = #'(0 . 0)
    }
  }
}
