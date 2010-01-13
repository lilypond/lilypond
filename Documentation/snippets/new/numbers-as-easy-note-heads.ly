\version "2.13.11"

\header {
  lsrtags = "pitches"
  texidoc = "
Easy notation note heads use the @code{note-names} property
of the @code{NoteHead} object to determine what appears inside
the note head.  By overriding this property, it is possible
to print numbers representing the scale-degree.

A simple engraver can be created to do this for every note head
object it sees.
"
  doctitle = "Numbers as easy note heads"
}

#(define Ez_numbers_engraver (list
  (cons 'acknowledgers
   (list
     (cons 'note-head-interface
       (lambda (engraver grob source-engraver)
         (let* (
           (context (ly:translator-context engraver))
           (tonic-pitch (ly:context-property context 'tonic))
           (tonic-name (ly:pitch-notename tonic-pitch))
           (grob-pitch (ly:event-property (event-cause grob) 'pitch))
           (grob-name (ly:pitch-notename grob-pitch))
           (delta (modulo (- grob-name tonic-name) 7))
           (note-names (make-vector 7 (number->string (+ 1 delta)))))
        (ly:grob-set-property! grob 'note-names note-names))))))))

\layout {
  \context {
    \Voice
    \consists \Ez_numbers_engraver
  }
}

\relative c' {
  \easyHeadsOn
  c4 d e f
  g4 a b c \break

  \key a \major
  a,4 b cis d
  e4 fis gis a \break

  \key d \dorian
  d,4 e f g
  a4 b c d
}
