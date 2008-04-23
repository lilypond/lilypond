\version "2.11.43"
\header {
  lsrtags = "expressive-marks,text,tweaks-and-overrides"
  texidoc = "
Using Scheme code to override the stencil for @code{MetronomeMark}
objects, this example allows the creation of metronome marks which include
text directions.  The function @code{\tempoChangeMarkup} is called with three
strings: the text label, note duration, and beats per minute.  To print the
new metronome mark, this is followed by the standard @code{\tempo} command.
"
  doctitle = "Adding text indications to metronome marks"
}

% Thanks to Alexander Kobel for this snippet

tempoMarkLabelSize = #0
tempoMarkNoteSize = #-6

#(define (tempoChangeMarkupFactory grob label noteValue tempo)
 (interpret-markup
  (ly:grob-layout grob)
  (ly:grob-alist-chain grob (ly:output-def-lookup (ly:grob-layout grob) 'text-font-defaults))
  (markup
   #:fontsize tempoMarkLabelSize #:italic #:concat (label (if (string-null? label) "(" " (" ))
   #:hspace -1
   #:fontsize tempoMarkNoteSize #:general-align Y DOWN #:note noteValue UP
   #:fontsize tempoMarkLabelSize #:italic #:concat( "= " tempo ")" )
  )
 ))

#(define (tempoChangeStencil label noteValue tempo)
 (lambda (grob)
  (tempoChangeMarkupFactory grob label noteValue tempo)
 ))

tempoChangeMarkup = #(define-music-function (parser location label noteValue tempo) (string? string? string?)
       #{
         \once \override Score.MetronomeMark #'stencil = #(tempoChangeStencil $label $noteValue $tempo)
       #})

\relative c' {
  \time 4/4
  \clef treble
  % initialize the override
  \tempoChangeMarkup #"Moderato" #"4" #"63"
  % markup is printed
  \tempo 4 = 63
  c4 d e f
  g a b c
  \time 6/4
  \mark \default
  \tempoChangeMarkup #"presto" #"2." #"90"
  \tempo 2. = 90
  c2. g \break
  e \tempoChangeMarkup #"handling collision with RehearsalMark" #"4" #"120" \tempo 4 = 120 c
  \time 4/4
  \mark \default
  c1
}
