\version "2.23.12"

\header {
  texidoc = "@code{grob::when} and @code{grob::rhythmic-location} also work on
dead grobs."
}

#(define (kill-and-show-location grob)
   (ly:grob-suicide! grob)
   (ly:message "Moment: ~a  Rhythmic location: ~a"
               (grob::when grob)
               (grob::rhythmic-location grob)))

{
  \override NoteHead.before-line-breaking = #kill-and-show-location
  c'1
  \override MultiMeasureRest.before-line-breaking = #kill-and-show-location
  R1
}
