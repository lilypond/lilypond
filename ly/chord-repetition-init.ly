%%% -*- Mode: Scheme -*-
\version "2.19.22"
%{
  Chord repetition behavior is not customizable in the parser.  That
  is due to it usually being done by the toplevel music handler
  affecting every bit of music at the same time, not closely related
  to music input.  Customized behavior is instead accomplished by
  calling \chordRepeats explicitly on some music list with a list of
  event types you wish to keep by default (if any events of that kind
  are found already on the repeat chord, however, they still get
  removed from the original).

  The default behavior is straightforward: don't keep anything but the
  rhythmic events themselves.
%}

chordRepeats =
#(define-music-function (event-types music)
   ((list? '()) ly:music?)
   "Walk through @var{music} putting the notes of the previous chord
into repeat chords, as well as an optional list of @var{event-types}
such as @code{#'(string-number-event)}."
   (expand-repeat-chords! (cons 'rhythmic-event event-types) music))

tabChordRepeats =
#(define-music-function (event-types music)
   ((list? '()) ly:music?)
   "Walk through @var{music} putting the notes, fingerings and string
numbers of the previous chord into repeat chords, as well as an
optional list of @var{event-types} such as @code{#'(articulation-event)}."
   #{ \chordRepeats
      #(append '(string-number-event fingering-event) event-types)
      #music
   #})

tabChordRepetition =
#(define-void-function () ()
   (_i "Include the string and fingering information in a chord repetition.
This function is deprecated; try using @code{\\tabChordRepeats} instead.")
   (ly:parser-define! '$chord-repeat-events
                      '(string-number-event fingering-event)))
