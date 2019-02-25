\version "2.19.21"

#(define (glissando::calc-extra-dy grob)
   (let* ((original (ly:grob-original grob))
          (left-bound (ly:spanner-bound original LEFT))
          (right-bound (ly:spanner-bound original RIGHT))
          (left-pitch (ly:event-property (event-cause left-bound) 'pitch))
          (right-pitch (ly:event-property (event-cause right-bound) 'pitch)))

     (if (and (= (ly:pitch-octave left-pitch) (ly:pitch-octave right-pitch))
              (= (ly:pitch-notename left-pitch) (ly:pitch-notename right-pitch)))
         (- (ly:pitch-alteration right-pitch) (ly:pitch-alteration left-pitch))
         0 )))

upper= \relative c' {
  \time 4/4
  \key e \major
  \set Staff.midiInstrument = "acoustic guitar (steel)"
  \set fingeringOrientations = #'(left)

  \partial 4. \acciaccatura c16 \glissando cis8 e4
  < cis-1 g'-3 >2 s8 \grace a16 ( \glissando < b-2 >8\3 ) < d-1 > ( b )
  < e-3 >\2 ( <d-1> b ) \grace < ais-2 >16 ( \glissando a8 g ) s4.
  s4. < d'\3 g\2 >8 < gis,\4  d'\3 fis\2 >2\arpeggio ~
  < gis\4 d'\3 fis\2 >2 < b'\2\harmonic e\harmonic >2\fermata
}

lower= \relative {
  \set fingeringOrientations = #'(left)

  \partial 4. s4.
  s4 e,4 s2
  s2 s8 < e'-3 >4. ~
  e4 \hideNotes \grace { b8 \glissando s4 } \unHideNotes < e-2 >4\5 e,2 ~
  2 < e'\6\harmonic >
}

\score {
  \new StaffGroup \with {instrumentName = "Guitar" } <<
    \new Staff = "guitar" <<
      \context Voice = "upper guitar" {
        \clef "G_8" \voiceOne
        \override Glissando.gap = #0.5
        \override Glissando.extra-offset = #'(-0.5 . 0)
        \override Glissando.springs-and-rods = #ly:spanner::set-spacing-rods
        \override Glissando.minimum-length = #4
        \override Glissando.extra-dy = #glissando::calc-extra-dy
        \upper
      }
      \context Voice = "lower guitar" {
        \clef "G_8" \voiceTwo
        \override Glissando.bound-details.right.padding = #1
        \override Glissando.bound-details.left.padding = #0.2
        \lower
      }
    >>
    \new TabStaff = "tab" <<
      \context TabVoice = "upper tab" { \clef "moderntab" \voiceOne \upper }
      \context TabVoice = "lower tab" { \clef "moderntab" \voiceTwo \lower }
    >>
  >>

  \layout {
    \context {
      \Staff
      \hide StringNumber
    }
    \context {
      \TabStaff
      \revert Arpeggio.stencil
    }
  }
}
