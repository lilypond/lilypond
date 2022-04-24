\version "2.21.0"


\paper {
  tagline = ##f
  paper-height = 70\mm
}


% This function makes all tuplet brackets horizontal.
horizontalTuplets =
  \override TupletBracket #'stencil =
    #(lambda (grob)
       (let* ((pos (ly:grob-property grob 'positions))
              (dir (ly:grob-property grob 'direction))
              (new-pos (if (= dir 1)
                           (max (car pos)(cdr pos))
                           (min (car pos)(cdr pos)))))
         (ly:grob-set-property! grob 'positions (cons new-pos new-pos))
         (ly:tuplet-bracket::print grob)))


% A function to display large and centered time signatures between the
% staves, using a text font.
largeTimeSignatures =
  \override TimeSignature.stencil =
    #(lambda (grob)
       (let ((fraction (ly:grob-property grob 'fraction '(4 . 4))))
         (grob-interpret-markup
           grob
           (markup #:override '(font-size . 6)
                   #:override '(font-name . "New Century Schoolbook")
                   #:override '(baseline-skip . 3.5)
                   #:column ((ly:number->string (car fraction))
                             (ly:number->string (cdr fraction)))))))


% Within a `PianoStaff' context, dynamic marks can't be moved to the left of
% the preceding bar line by default (to avoid collisions with bar lines).
% The following macro overrides this, assigning a zero horizontal width to
% the dynamic mark.
noHorzSpace =
  \tweak DynamicText.extra-spacing-width #'(+inf.0 . -inf.0) \etc


% A tweak-like function to move dynamics.
moveDyn =
  #(define-event-function (x y event) (number? number? ly:event?)
    #{ \tweak DynamicLineSpanner.outside-staff-priority ##f
       \offset DynamicText.X-offset #x
       \offset DynamicLineSpanner.Y-offset #y
       #event #})


% Put the change clef after the bar line and time signature.
clefAfterBarline = {
  \once \override Score.TimeSignature.space-alist.clef =
    #'(minimum-space . 0)
  \once \override Score.BreakAlignment.break-align-orders =
    #(make-vector 3 '(staff-bar
                      time-signature
                      clef)) }

% Color shorthands.
blue = \override NoteHead.color = #blue
red = \override NoteHead.color = #red


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


RH = {
  \set subdivideBeams = ##t
  \set strictBeatBeaming = ##t
  \override TupletBracket.bracket-visibility = ##t
  \override TupletBracket.outside-staff-priority = #3
  \horizontalTuplets
  \tupletUp

  % 1
  \clef "treble"
  \red
  \tweak style #'dashed-line \tuplet 3/2 {
    \tuplet 3/2 { d'4\noHorzSpace \moveDyn -1.5 -3.2 ^\ff r8 }
    r8 } |

  % 2
  \blue
  \tuplet 5/4 {
    b'''16[\noHorzSpace \moveDyn -3.7 4 ^\mf
    a''16.~\moveDyn 0 0.5 ^\ff }
  \tuplet 5/4 {
    a''32
    \red
    d'''8 \moveDyn 0 0 ^\ff }
  \tuplet 5/4 {
    \blue
    r16. <g'' aes'''>16]\moveDyn -0.2 -0.5 ^\p } |

  % XXX Currently, cross-staff beaming support is partially broken.  This
  %     means we have to apply some work-arounds.
  %
  %     - The `outside-staff-priority' property of tuplet brackets must be
  %       set to `#f'; any other value makes the cross-staff beaming go
  %       crazy.
  %     - Vertical position adjustments are taken relative to the other
  %       staff; this means that they are sensitive to staff distances (as
  %       set by `staff-staff-spacing', for example).

  % 3
  \once \override TupletBracket.outside-staff-priority = ##f
  \tweak text #tuplet-number::calc-fraction-text
  \tweak style #'dashed-line
  \tweak positions #'(20.5 . 20.5) \tuplet 4/3 {
    \tweak positions #'(18 . 18) \tuplet 5/4 {
      \red
      \change Staff = "LH"
      \stemUp
      <d'' es'>8\tweak positions #'(5 . 7) [_\pp_\laissezVibrer
      \change Staff = "RH"
      \stemDown
      f''32\moveDyn -2 0 ^\f_\laissezVibrer
    }
    \blue
    b''32\moveDyn 0 0.2 ^\ff_\laissezVibrer
    a'16.\moveDyn 0 -0.4 ^\ff_\laissezVibrer
    gis''8\moveDyn 0 0.2 ^\ff_\laissezVibrer
    \change Staff = "LH"
    \clef "bass"
    \red
    \stemUp
    cis'8]_\pp
  } |

  % 4
  \change Staff = "RH"
  \stemNeutral
  c''4.~_\p
  \tuplet 5/4 { c''32 r8 } |
}


LH = {
  \set subdivideBeams = ##t
  \set strictBeatBeaming = ##t
  \override TupletBracket.bracket-visibility = ##t
  \override TupletBracket.outside-staff-priority = #3
  \horizontalTuplets

  % 1
  \clef "bass"
  \tupletDown
  << { \red
       ees4\moveDyn -0.5 0 ^\f }
  \\ { \red
       \tweak style #'dashed-line \tuplet 3/2 {
         f,4~\noHorzSpace \moveDyn -2 -2 _\f
         \tuplet 3/2 { f,16 r8 } } }
  >> |

  % 2
  \tupletUp
  \clefAfterBarline \clef "treble"
  \tuplet 5/4 {
    \blue
    \set stemRightBeamCount = #1
    r16.[ gis'16~_\f }
  \tuplet 5/4 {
    gis'32
    \red
    c'8\moveDyn 0 -0.4 _\pp }
  \tuplet 5/4 {
    cis''16. _\mf
    \blue
    fis'16] _\p } |

  % 3
  \textSpannerDown
  \override TextSpanner.bound-details.left.text =
    \markup { \musicglyph "pedal.Ped" }
  \override TextSpanner.bound-details.right.text =
    \markup { \musicglyph "pedal.*" }
  \override TextSpanner.dash-fraction = #0.1
  \override TextSpanner.dash-period = #1.5
  s4\startTextSpan s16. s32\stopTextSpan |

  % 4
  \red
  d,4.~_\mf \tuplet 5/4 { d,32 r8 } |
}


\score {
  \new PianoStaff <<
    \new Staff = "RH" {
      \hide Staff.TimeSignature
      % Give more horizontal space for the (hidden) time signature.
      \override Staff.TimeSignature.extra-spacing-width = #'(0.0 . 2.0)
      \accidentalStyle dodecaphonic
      \RH
    }

    \new Dynamics \with { \consists Time_signature_engraver }
    {
      \largeTimeSignatures

      \time 2/8 s4 |
      \time 3/8 s4. |
      s4. |
      \time 4/8 s2 |
    }

    \new Staff = "LH" {
      \hide Staff.TimeSignature
      \accidentalStyle dodecaphonic
      \LH
    }
  >>
}
