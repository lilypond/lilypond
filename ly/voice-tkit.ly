%\version "2.19.19"

\include "base-tkit.ly"

make-voice =
#(define-music-function (parser location name) (voice-prefix?)
   (define music (make-id name "Music"))
   (if music
       #{
         \new Voice = #(string-append name "Voice") <<
           #(if KeepAlive KeepAlive)
           #(if Time Time )
           #music
         >>
       #} ))

