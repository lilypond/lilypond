\version "2.21.5"

\header {
  texidoc = "Isolated percent-repeat signs can be printed."
}

\layout {
  ragged-right =##t
}

%% This is based on the snippet isolated-percent-repeats.ly.
percent =
#(define-music-function (dur) (ly:duration?)
   "Make a percent repeat."
   (make-music 'PercentEvent
               'length (ly:duration-length dur)))

\relative c'' <<
  \new Staff { \percent 1 }
  \new Staff { \percent 2 \percent 2 | \percent \breve }
  %% \new Voice tests initialization of the engraver in mid score.
  \new Staff { R1 \new Voice { \percent 1 } }
  \new Staff { R1 \new Voice { \percent 2 \percent 2 } }
>>
