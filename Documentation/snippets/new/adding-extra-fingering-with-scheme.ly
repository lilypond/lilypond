\version "2.16.0"

\header {
  lsrtags = "scheme-language"

  texidoc = "
You can add additional elements to notes using @code{map-some-music}. In this
example, an extra script is attached to a note.

In general, first do a @code{\\displayMusic} of the music you want to
create, then write a function that will work on the appropriate parts
of the music for you.
"
  doctitle = "Adding extra fingering with scheme"
}

addScript =
#(define-music-function (parser location script music)
   (ly:event? ly:music?)
   (map-some-music
    (lambda (mus)
      (define (append-script-at! prop)
        (set! (ly:music-property mus prop)
              (append (ly:music-property mus prop)
                      (list (ly:music-deep-copy script))))
        mus)
      (case (ly:music-property mus 'name)
        ((EventChord)
         (append-script-at! 'elements))
        ((NoteEvent)
         (append-script-at! 'articulations))
        (else #f)))
    music))

\score {
  {
    \addScript _6 { c'4-3 <c' e' g'> }
  }
}
