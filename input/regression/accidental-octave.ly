
\version "2.1.22"

\header {
texidoc="
This shows how accidentals in different octaves are handled.
(DOCME)
"


}

#(define  (lo-octave p)
  (let* ((a (ly:pitch-alteration p))
         (n (ly:pitch-notename p)))
    (ly:make-pitch -1 n a)))

#(define (no-octaves music)
  (let* ((es (ly:get-mus-property music 'elements))
         (e (ly:get-mus-property music 'element))
         (p (ly:get-mus-property music 'pitch)))

    (if (pair? es)
        (ly:set-mus-property!
         music 'elements
         (map no-octaves es)))

    (if (ly:music? e)
        (ly:set-mus-property!
         music 'element
         (no-octaves e)))

    (if (ly:pitch? p)
        (begin
          (set! p (lo-octave p))
          (ly:set-mus-property! music 'pitch p)))


    music))



mel = \notes \transpose c c' {
  \time 4/4 \key d \major
  gis4 g' g gis' | gis2 g' | g1 | gis | g | gis' | g |
  fis4 f' f fis' | fis2 f' | f1 | fis | f | fis' | f |
  \bar "|." \break
}

\score {
  << \context Staff \mel
     \context NoteNames \apply #no-octaves \mel
  >>
}

