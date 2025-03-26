\version "2.24.4"

\header {
  texidoc = "Top-level music with the @code{void} property set is discarded.
This test should show just the message, ``There are no scores.''"
}

#(ly:set-option 'warning-as-error #t)

testVoidEvent = $(make-music 'Event 'void #t)
testVoidMusic = $(make-music 'Music 'void #t)
testVoidSequentialMusic = $(let ((mus #{ c d e #} ))
                            (set! (ly:music-property mus 'void) #t)
                            mus)

\book {
  \markup "There are no scores."

  #(make-music 'Event 'void #t)
  $(make-music 'Event 'void #t)
  \testVoidEvent
  #(make-music 'Music 'void #t)
  $(make-music 'Music 'void #t)
  \testVoidMusic
  #(make-music 'SequentialMusic 'void #t)
  $(make-music 'SequentialMusic 'void #t)
  \testVoidSequentialMusic

  \bookpart {
    #(make-music 'Event 'void #t)
    $(make-music 'Event 'void #t)
    \testVoidEvent
    #(make-music 'Music 'void #t)
    $(make-music 'Music 'void #t)
    \testVoidMusic
    #(make-music 'SequentialMusic 'void #t)
    $(make-music 'SequentialMusic 'void #t)
    \testVoidSequentialMusic
  }
}

#(make-music 'Event 'void #t)
$(make-music 'Event 'void #t)
\testVoidEvent
#(make-music 'Music 'void #t)
$(make-music 'Music 'void #t)
\testVoidMusic
#(make-music 'SequentialMusic 'void #t)
$(make-music 'SequentialMusic 'void #t)
\testVoidSequentialMusic
