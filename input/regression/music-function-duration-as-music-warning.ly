\version "2.27.4"

\header {
  texidoc = "When music functions interpret a duration as music right after
accepting a pitch argument, chances are that the duration actually was entered
in accident.  Checks that the expected warnings are emitted.  No score is
output, but there is informational output about the number of music expressions
seen by LilyPond's parser for several constructs."
}

#(ly:expect-warning (G_ "Duration following pitch taken as music"))

count-expressions =
#(define-scheme-function (m) (ly:music?)
  (assert (music-is-of-type? m 'sequential-music))
  (let ((m (ly:music-property m 'elements)))
   (ly:music-message (car m) "~a"
    (format #f "~d music expression~:p seen" (length m)))))

\count-expressions {
  \relative c' { c4 d e f }
}

\count-expressions {
  \relative c'1 { c4 d e f }
}

\count-expressions {
  \relative 1
}

\count-expressions {
  \transpose c c' { c4 d e f }
}

#(ly:expect-warning (G_ "Duration following pitch taken as music"))

\count-expressions {
  \transpose c c'1 { c4 d e f }
}

#(ly:expect-warning (G_ "Duration following pitch taken as music"))

\count-expressions {
  \fixed c'1 { c4 d e f }
}
