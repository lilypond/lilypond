
\version "1.5.68"

\header {
texidoc="
This shows how accidentals in different octaves are handled.
FIXME: Shorten and docu

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


\include "paper16.ly"

mel = \notes \transpose c'' {
  \time 4/4 \key d \major
  gis4 g' g gis' | gis2 g' | g1 | gis | g | gis' | g |
  fis4 f' f fis' | fis2 f' | f1 | fis | f | fis' | f |
  \bar "|." \break
}

mus = \notes {
   \mel

%% FIXME.
%   \property Score.oneMeasureLazy = ##t


   \property Score.autoAccidentals = #'(Staff (same-octave . 0))
   < s1^""^""^"$\\backslash$property Score.autoAccidentals = \\#'(Staff (same-octave . 0))" \mel >
   \property Score.autoAccidentals = #'(Staff (same-octave . 1))
   < s1^""^""^"$\\backslash$property Score.autoAccidentals = \\#'(Staff (same-octave . 1))" \mel >
   \property Score.autoAccidentals = #'(Staff (any-octave . 0))
   < s1^""^""^"$\\backslash$property Score.autoAccidentals = \\#'(Staff (any-octave . 0))" \mel >
   \property Score.autoAccidentals = #'(Staff (any-octave . 1))
   < s1^""^""^"$\\backslash$property Score.autoAccidentals = \\#'(Staff (any-octave . 1))" \mel >
   \modernAccidentals
   < s1^""^""^"$\\backslash$modernAccidentals" \mel >
   \modernCautionaries
   < s1^""^""^"$\\backslash$modernCautionaries" \mel >
   \noResetKey
   < s1^""^""^"$\\backslash$noResetKey" \mel >
   \forgetAccidentals
   < s1^""^""^"$\\backslash$forgetAccidentals" \mel >
}


\score {
  < \context Staff \mus
    \context NoteNames \apply #no-octaves \mus
  >
  \paper {
    indent=0.0
  }
}
