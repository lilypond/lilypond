\version "2.1.25"

\header{
texidoc="
This shows how accidentals are handled.
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


mel = \notes { \key d \major \time 4/4
 d4  dis dis8 dis, d4 | d dis disis8 d, dis4 | d des disis8 dis, d4 | dis deses d dis ~ | dis dis ~ dis8 d, dis4 ~ | \break
 dis dis cis c | c cis cisis cis | c ces cisis c | cis ceses c cis ~ | cis cis ~ cis cis \bar "|."  | \break
}

\score { \notes
 <<
  \context Staff \transpose c c'' \mel
  \context NoteNames  \apply #no-octaves \mel
 >>
}

