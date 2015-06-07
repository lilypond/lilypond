%\version "2.19.22"

\include "base-tkit.ly"

make-voice =
#(define-music-function (name) (voice-prefix?)
   (define music (make-id name "Music"))
   (if music
       #{
         \new Voice = #(string-append name "Voice") <<
           #(if KeepAlive KeepAlive)
           #(if Time Time )
           #music
         >>
       #} ))

