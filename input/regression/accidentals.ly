\version "1.5.68"

\header{
texidoc="
This shows how accidentals are handled.
"
}

#(define  (lo-octave p)
  (let* ((a (pitch-alteration p))
         (n (pitch-notename p)))
    (make-pitch -1 n a)))

#(define (no-octaves music)
  (let* ((es (ly-get-mus-property music 'elements))
         (e (ly-get-mus-property music 'element))
         (p (ly-get-mus-property music 'pitch)))

    (if (pair? es)
        (ly-set-mus-property!
         music 'elements
         (map no-octaves es)))

    (if (music? e)
        (ly-set-mus-property!
         music 'element
         (no-octaves e)))

    (if (pitch? p)
        (begin
          (set! p (lo-octave p))
          (ly-set-mus-property! music 'pitch p)))


    music))


mel = \notes { \key d \major \time 4/4
 d4  dis dis8 dis, d4 | d dis disis8 d, dis4 | d des disis8 dis, d4 | dis deses d dis ~ | dis dis ~ dis8 d, dis4 ~ | \break
 dis dis cis c | c cis cisis cis | c ces cisis c | cis ceses c cis ~ | cis cis ~ cis cis \bar "|."  | \break
}

\score { \notes
 <
  \context Staff \transpose c''' {
   \mel
   \property Score.oneMeasureLazy = ##t
   \property Score.autoAccidentals = #'(Staff (same-octave . 0))
   < s1^"$\\backslash$property Score.autoAccidentals = \\#'((same-octave . 0))" \mel >
   \property Score.autoAccidentals = #'(Staff (same-octave . 1))
   < s1^"$\\backslash$property Score.autoAccidentals = \\#'((same-octave . 1))" \mel >
   \property Score.autoAccidentals = #'(Staff (any-octave . 0))
   < s1^"$\\backslash$property Score.autoAccidentals = \\#'((any-octave . 0))" \mel >
   \property Score.autoAccidentals = #'(Staff (any-octave . 1))
   < s1^"$\\backslash$property Score.autoAccidentals = \\#'((any-octave . 1))" \mel >
   \modernAccidentals
   < s1^"$\\backslash$modernAccidentals" \mel >
   \modernCautionaries
   < s1^"$\\backslash$modernCautionaries" \mel >
   \noResetKey
   < s1^"$\\backslash$noResetKey" \mel >
   \forgetAccidentals
   < s1^"$\\backslash$forgetAccidentals" \mel >
  }
  \context NoteNames \repeat unfold 9 \apply #no-octaves \mel
 >
 \paper {
  indent = 0.0
 }
}

