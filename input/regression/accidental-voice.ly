\version "1.5.43.rz1"

\header{
texidoc="
This shows how modern cross voice auto cautionary accidentals are handled.
The first two fisses get accidentals because they belong to different voices.
The first f gets cautionary natural because of previous measure.
The last f gets cautionary natural because fis was only in the other voice.
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
        (ly-set-mus-property
         music 'elements
         (map no-octaves es)))

    (if (music? e)
        (ly-set-mus-property
         music 'element
         (no-octaves e)))

    (if (pitch? p)
        (begin
          (set! p (lo-octave p))
          (ly-set-mus-property music 'pitch p)))


    music))

voicea = \notes \transpose c'' {
    \stemUp
    fis2 a2 f4 fis a2
}
voiceb = \notes \transpose c'' {
    \stemDown
    c2 fis2  f4 c   f2
}

\score {
    <
	\notes
	\context NoteNames=namesa \apply #no-octaves \voicea
	\context Staff < 
	    \modernVoiceCautionaries
	    \context Voice = voicea \voicea
	    \context Voice = voiceb \voiceb
	>
	\context NoteNames=namesb \apply #no-octaves \voiceb
    >
}
