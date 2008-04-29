\version "2.11.45"
\header {
  lsrtags = "expressive-marks,tweaks-and-overrides"
  texidoc = "
Using Scheme code to override the stencil for @code{MetronomeMark}
objects, this example allows the creation of metronome marks which include
text directions.  The function @code{\movement} is called with three
arguments: the text label, note duration, and beats per minute.
"
  doctitle = "Adding text indications to metronome marks"
}

#(define-markup-command (mvt layout props arg) (markup?)
  (interpret-markup layout props
     (markup #:huge #:bold arg)))

#(define (string->duration duration-string)
  "Parse the `duration-string', e.g. ''4..'' or ''breve.'', and return a duration object."
  (let* ((length (string-length duration-string))
         (dot-index (or (string-index duration-string #\.) length))
         (len (substring duration-string 0 dot-index))
         (dots (- length dot-index)))
   (ly:make-duration (cond ((string=? len "breve") -1)
                           ((string=? len "longa") -2)
                           ((string=? len "maxima") -3)
                           (else (log2 (string->number len))))
                     dots 1 1)))

movement = #(define-music-function (parser location text duration count music)
                        (string? string? integer? ly:music?)
   (define (format-movement-markup dur count context)
     (markup #:mvt text #:hspace 1
             #:concat ("(" #:general-align Y DOWN #:smaller #:note duration 1)
             "="
             #:concat ((number->string count) ")")))
  #{
    \set Score.metronomeMarkFormatter = #$format-movement-markup
    \set Score.tempoWholesPerMinute = #$(ly:moment-mul (ly:make-moment count 1)
                                         (ly:duration-length
                                           (string->duration duration)))
    \set Score.tempoUnitDuration = #$(string->duration duration)
    \set Score.tempoUnitCount = $count
    $music
    \set Score.metronomeMarkFormatter = #format-metronome-markup
  #})

\layout { ragged-right = ##f }

\relative c' { 
  \time 3/4
  \movement "Allegro" "2." #92
  c2 e4
  g2.
  \movement "Moderato" "4" #104
  f4 e d
  \tempo 4 = 92
  c2.
}
