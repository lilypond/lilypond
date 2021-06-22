\version "2.23.3"

\header {
  texidoc = "@code{music-map} also recurses into @code{articulations}."
}

\new Staff \with {
  instrumentName = \markup { Should be \dynamic pp }
}
$(music-map
   (lambda (m)
     (if (music-is-of-type? m 'absolute-dynamic-event)
         pp
         m))
   #{ c\ff #})
